#!/usr/bin/env python3
# Ports macwire's compile-and-run test cases (tests/src/test/resources/test-cases/) into kindlings-di's test suites
# (di/src/test/scala*/hearth/kindlings/di/upstream/Macwire*TestCasesSpec.scala). Run `sbt --client di/scalafmtAll`
# afterwards. Usage, from the repository root:
#   gh api repos/softwaremill/macwire/tarball/v2.6.7 > target/macwire.tgz
#   mkdir -p target/macwire && tar xzf target/macwire.tgz -C target/macwire --strip-components=1
#   python3 scripts/port-macwire-test-cases.py target/macwire
import os, re, sys
up = os.path.abspath(sys.argv[1]) + '/'
out = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'di', 'src', 'test') + '/'
inc_dir = up + 'test-util/src/main/resources/include/'
cases_dir = up + 'tests/src/test/resources/test-cases/'

def valueNotFound(c): return f"Cannot find a value of type: [{c}]"
def ambiguous(c): return f"Found multiple values of type [{c}]"

# expected failure fragments, per Scala version (from macwire's CompileTests / AutowireCompileTests)
fail2 = {
  "companionFactoryAmbiguous": ["No public primary constructor found for", "PrivConstructor", "and multiple matching apply methods in its companion object were found."],
  "explicitDepsNotWiredWithImplicitVals": [valueNotFound("A")],
  "explicitDepsWiredWithImplicitValsFromMethodScope": [ambiguous("A"), "dependency", "implicitDependencyA"],
  "importAmbiguous": [ambiguous("A"), "myA", "theA"],
  "nestedMethodsWired": [ambiguous("A"), "outerA", "innerA"],
  "nestedWithManyMatchingParamsWired": [ambiguous("A"), "a1", "a2", "a3"],
  "multipleMethodParameters": [ambiguous("A"), "a1", "a2"],
  "simpleValsMissingValue": [valueNotFound("B")],
  "simpleValsDuplicateValue": [ambiguous("B"), "theB1", "theB2"],
  "secondaryConstructorNotAnnotated": [valueNotFound("String")],
  "phantomConstructor": ["Cannot find a public constructor", "[Target]"],
  "companionObjectHasNoMethods": ["has no apply methods constructing target type", "Target]"],
  "companionObjectHasFakeApplyMethods": ["has no apply methods constructing target type", "Target]"],
  "toManyInjectAnnotations": ["Ambiguous constructors annotated with @javax.inject.Inject or @jakarta.inject.Inject for type [Target]"],
  "nullaryMethodUsedAsCandidate": [ambiguous("A")],
  "wireWithTwoParamsLists": [],
  "wireRecEmptyString": [valueNotFound("String")],
}
# kindlings words some errors differently from macwire (same information): the fragments to look for instead
wording = {
  "companionFactoryAmbiguous": ["No public primary constructor found for", "PrivConstructor", "and multiple matching apply methods in its companion object were found for"],
  "phantomConstructor": ["No public primary constructor found for [Target]", "has no apply methods constructing target type"],
  "unusedDependency": ["unused dependencies: C()"],
  "autowireMembersOfDuplicate": ["duplicate type in dependencies list: A, for: new A"],
}
auto = {
  "privateConstructor": ["cannot find a provided dependency, public constructor or public apply method for: A;", "wiring path: A"],
  "cyclicDependency": ["cyclic dependencies detected;", "wiring path: A -> B -> A"],
  "primitives": ["cannot use a primitive type or String in autowiring;", "wiring path: A -> String"],
  "unusedDependency": ["unused dependencies: C.apply()"],
  "duplicate": ["duplicate type in dependencies list: A, for: a"],
  "plainTrait": ["cannot find a provided dependency, public constructor or public apply method for: A;", "wiring path: B -> A"],
  "autowireMembersOfDuplicate": ["duplicate type in dependencies list: A, for: new A()"],
}

ascriptions = [
  ('val component = wireWith(Component.create _)', 'val component: Component = wireWith(Component.create _)'),
  ('def someMethod(dependency: Dependency)(implicit implicitDependency: Dependency) = {',
   'def someMethod(dependency: Dependency)(implicit implicitDependency: Dependency): Unit = {'),
  ('  def create() = {\n    val s = "foo"', '  def create(): A = {\n    val s = "foo"'),
  ('def theC(a: A, b: B) = Props(wire[C])', 'def theC(a: A, b: B): C = Props(wire[C])'),
  ('def someMethod(dependency: A)(implicit implicitDependencyA: A, implicitDependencyB: B) = {',
   'def someMethod(dependency: A)(implicit implicitDependencyA: A, implicitDependencyB: B): Unit = {'),
  ('  def a(outerA: A) = {', '  def a(outerA: A): A => B = {'),
]
# cases whose objects must stay local to the block: a block-level implicit must not be visible inside them
no_hoist = {'implicitDepsWiredWithImplicitValsFromMethodScope', 'methodWithSingleImplicitParamOk'}

def load(path):
    src = open(path).read()
    def repl(m):
        return open(inc_dir + m.group(1)).read()
    src = re.sub(r'#include ([a-zA-Z]+)', repl, src)
    src = src.replace('import com.softwaremill.macwire._', '').replace('import com.softwaremill.macwire.*', '')
    src = src.replace('com.softwaremill.macwire.Module', 'hearth.kindlings.di.Module')
    src = braces(src)
    src = re.sub(r'(?m)^(\s*)assert\(', r'\1require(', src)
    # Scala 2: an untyped `val x = wire[T]` in a class/object fails with "recursive value x needs type" (Hearth reads
    # the enclosing scope's member types eagerly, docs/research/hearth-bug-scala2-untyped-val-enclosing-scope.md): the
    # ported cases get the type ascription, as in the rest of kindlings-di's tests.
    src = re.sub(r'(?m)^(\s*(?:private )?(?:lazy val|val|def) [A-Za-z0-9_]+(?:\([^)]*\))*) = (wire|wireRec)\[(.+)\][ \t]*$', r'\1: \3 = \2[\3]', src)
    for old, new in ascriptions:
        src = src.replace(old, new)
    return src

def braces(code):
    out, lines, i = [], code.split('\n'), 0
    while i < len(lines):
        l = lines[i]
        m = re.match(r'^(\s*)((?:case )?class .*\)|trait \w+.*|object \w+.*):\s*$', l)
        if m:
            base = len(m.group(1))
            out.append(m.group(1) + m.group(2) + ' {')
            i += 1
            while i < len(lines) and (not lines[i].strip() or len(lines[i]) - len(lines[i].lstrip()) > base):
                out.append(lines[i]); i += 1
            out.append(m.group(1) + '}')
            continue
        out.append(l); i += 1
    return '\n'.join(out)

def indent(code, n):
    return '\n'.join((' ' * n + l) if l.strip() else '' for l in code.split('\n'))

def name_of(f):
    for suf in ['.success', '.failure', '.warning']:
        if f.endswith(suf):
            return f[:-len(suf)], suf[1:]
    return None, None

# macwire test cases kindlings-di does not support, with the reason (documented in docs/user-guide/di.md, "Differences
# from macwire"): ported as tests asserting the compilation error, so that a fix shows up as a failing test.
differences = {
  'anonFuncAndMethodsArgsWiredOk': 'parameters of enclosing lambdas are not candidates',
  'anonFuncArgsWiredOk': 'parameters of enclosing lambdas are not candidates',
  'nestedAnonFuncsWired': 'parameters of enclosing lambdas are not candidates',
  'functionApplication': 'parameters of enclosing lambdas are not candidates',
  'nestedMethodsWired': 'parameters of outer enclosing methods are not candidates',
  'methodWithWiredWithinIfThenElse': 'parameters of outer enclosing methods are not candidates',
  'methodWithWiredWithinPatternMatch': 'values bound by a pattern are not candidates',
  'simpleValsInClassParameters': 'constructor parameters of the enclosing class that are not vals are not candidates',
  'inheritancePublicMembersInParent': 'inherited members are in the same scope as declared ones (ambiguity)',
  'import': 'members brought in by an import are not candidates',
  'importRename': 'members brought in by an import are not candidates',
  'importWildcard': 'members brought in by an import are not candidates',
  'importWildcardDoNotImportObjectMethods': 'members brought in by an import are not candidates',
  'importWildcardVisibility': 'members brought in by an import are not candidates',
  'importImplicit': 'members brought in by an import are not candidates',
  'methodContainingValDef': 'local vals of a nested block are not candidates',
  'callByNameWireWith': 'wireWith of a method with a by-name parameter, eta-expanded on Scala 2',
}
# cases that compile but give a different result, for a documented difference: (old, new) rewrites of the check
rewrites = {
  'wireSet': ('require(app.group.as == Set(plugin1.a1, plugin2.a2, app.a3, app.a4))',
              '// kindlings: members brought in by an import (`import plugin1._`) are not candidates (documented difference)\n'
              'require(app.group.as == Set(app.a3, app.a4))'),
  'wireList': ('require(app.group.as == List(plugin1.a1, plugin2.a2, app.a3, app.a4))',
               '// kindlings: members brought in by an import (`import plugin1._`) are not candidates, and the members declared\n'
               '// in the class come before the inherited ones (macwire: inherited first) (documented differences)\n'
               'require(app.group.as == List(app.a4, app.a3))'),
}
# failure cases whose expected error is a consequence of a documented difference: the case still does not compile,
# with kindlings' own error
failure_differences = {
  'importAmbiguous': 'members brought in by an import are not candidates',
  'nestedWithManyMatchingParamsWired': 'parameters of outer enclosing methods are not candidates',
}
# failure cases kindlings compiles, for a documented difference
compiles_differences = {
  'nullaryMethodUsedAsCandidate': 'members brought in by an import are not candidates: no ambiguity',
  'nestedMethodsWired': 'parameters of outer enclosing methods are not candidates: the inner parameter is used, no ambiguity',
}
# local vals are candidates on Scala 2 only (Hearth cannot see the method body being compiled on Scala 3)
scala3_local_vals = {'t103', 'wireWithImplicits'}

def hoisted(name, code):
    if name in no_hoist: return False
    # A case defining objects is about wiring from object members: it becomes an object of the companion (initialized
    # by the test), because a LOCAL object referring to itself during its own initialization (`Test.a` inside `Test`)
    # recurses forever. Cases without objects stay blocks (method scope: parameters, local vals).
    return re.search(r'(?m)^object ', code) is not None

def ident(name):
    return 'case_' + re.sub(r'[^A-Za-z0-9]', '_', name)

def emit(cls, header_comment, entries):
    objects = []
    for (name, kind, code, frags) in entries:
        reason = differences.get(name.split('/')[-1]) or ('local vals are not candidates on Scala 3' if cls.endswith('Scala3TestCasesSpec') and name in scala3_local_vals else None)
        if kind in ('success', 'warning') and not reason and hoisted(name, code):
            objects.append(f'  object {ident(name)} {{')
            objects.append(indent(code.rstrip(), 4))
            objects.append('  }')
            objects.append('')
        elif kind in ('success', 'warning') and not reason:
            objects.append(f'  def {ident(name)}(): Unit = {{')
            objects.append(indent(code.rstrip(), 4))
            objects.append('    ()')
            objects.append('  }')
            objects.append('')
    lines = ['package hearth.kindlings.di.upstream', '', 'import hearth.kindlings.di.*', '',
             header_comment,
             '@scala.annotation.nowarn', f'final class {cls} extends hearth.MacroSuite {{', '',
             '  // macwire\'s cases declare their classes in a block, so its messages show simple names: drop the prefix of the',
             '  // classes declared in this suite (and `java.lang.`) before looking for macwire\'s fragments',
             '  private def simpleNames(errors: String): String =',
             '    errors.replaceAll("\\u001b\\\\[[0-9;]*m", "").replaceAll("hearth\\\\.kindlings\\\\.di\\\\.upstream\\\\.[A-Za-z0-9_]+\\\\.", "").replace("java.lang.", "")',
             '']
    for (name, kind, code, frags) in entries:
        reason = differences.get(name.split('/')[-1]) or ('local vals are not candidates on Scala 3' if cls.endswith('Scala3TestCasesSpec') and name in scala3_local_vals else None)
        if kind in ('success', 'warning') and reason:
            esc = code.replace('"""', '\\"\\"\\"')
            lines.append(f'  test("{name} (documented difference: {reason})") {{')
            lines.append('    val errors: String = compileErrors("""')
            lines.append(indent(esc.rstrip(), 6))
            lines.append('    """)')
            lines.append('    assert(errors.nonEmpty, "this macwire case now compiles: move it back to the supported cases")')
            lines.append('  }')
        elif kind in ('success', 'warning') and hoisted(name, code):
            lines.append(f'  test("{name}") {{')
            lines.append(f'    val _ = {cls}.{ident(name)}')
            lines.append('  }')
        elif kind in ('success', 'warning'):
            lines.append(f'  test("{name}") {{')
            lines.append(f'    {cls}.{ident(name)}()')
            lines.append('  }')
        else:
            esc = code.replace('"""', '\\"\\"\\"')
            lines.append(f'  test("{name} (does not compile)") {{')
            lines.append('    val errors: String = compileErrors("""')
            lines.append(indent(esc.rstrip(), 6))
            lines.append('    """)')
            lines.append('    assert(errors.nonEmpty, "expected a compilation error")')
            for f in frags:
                fe = f.replace('\\', '\\\\').replace('"', '\\"')
                lines.append(f'    assert(simpleNames(errors).contains("{fe}"), errors)')
            lines.append('  }')
        lines.append('')
    lines.append('}')
    if objects:
        lines.append('')
        lines.append('@scala.annotation.nowarn')
        lines.append(f'object {cls} {{')
        lines.extend(objects)
        lines.append('}')
    return '\n'.join(lines) + '\n'

shared, s2, s3, jvm = [], [], [], []
# macwire's request/session scopes need JVM proxies: not part of kindlings-di (documented difference)
skipped = {'simpleWithAbstractScope'}
# need javax.inject on the classpath: a JVM-only artifact
jvm_only = {'phantomConstructor', 'secondaryConstructorAnnotated', 'secondaryConstructorNotAnnotated', 'toManyInjectAnnotations'}
for f in sorted(os.listdir(cases_dir)):
    p = cases_dir + f
    if not os.path.isfile(p): continue
    base, kind = name_of(f)
    if base is None: continue
    target = shared
    name = base
    if base.endswith('.scala2'): target = s2; name = base[:-7]
    elif base.endswith('.scala3'): target = s3; name = base[:-7]
    if name in skipped: continue
    if name in jvm_only: target = jvm
    if name in scala3_local_vals and target is shared:
        code = load(p)
        s2.append((name, kind, code, []))
        s3.append((name, kind, code, []))
        continue
    frags = (wording.get(name) or fail2.get(name, [])) if kind == 'failure' else []
    code0 = load(p)
    if name in rewrites:
        old, new = rewrites[name]
        assert old in code0, name
        code0 = code0.replace(old, new)
        target.append((name + ' (documented difference: imports)', kind, code0, frags))
        continue
    if kind == 'failure' and name in compiles_differences:
        target.append((name + ' (documented difference: ' + compiles_differences[name] + ')', 'success', code0, []))
        continue
    if kind == 'failure' and name in failure_differences:
        frags = []
        name = name + ' (documented difference: ' + failure_differences[name] + ')'
    target.append((name, kind, load(p), frags))

autos = []
for f in sorted(os.listdir(cases_dir + 'autowire')):
    base, kind = name_of(f)
    if base is None: continue
    frags = (wording.get(base) or auto.get(base, [])) if kind == 'failure' else []
    autos.append(('autowire/' + base, kind, load(cases_dir + 'autowire/' + f), frags))

hdr = ('// Ported from macwire 2.6.7 (Apache License 2.0), tests/src/test/resources/test-cases/: every test case is a test\n'
       '// body (macwire evaluates each file as a block), `com.softwaremill.macwire._` is replaced by `hearth.kindlings.di._`,\n'
       '// and a `.failure` case checks the fragments macwire\'s CompileTests expects in the error.')
os.makedirs(out + 'scala/hearth/kindlings/di/upstream', exist_ok=True)
os.makedirs(out + 'scalajvm/hearth/kindlings/di/upstream', exist_ok=True)
open(out + 'scalajvm/hearth/kindlings/di/upstream/MacwireJvmTestCasesSpec.scala', 'w').write(emit('MacwireJvmTestCasesSpec', hdr, jvm))
os.makedirs(out + 'scala-2/hearth/kindlings/di/upstream', exist_ok=True)
os.makedirs(out + 'scala-3/hearth/kindlings/di/upstream', exist_ok=True)
open(out + 'scala/hearth/kindlings/di/upstream/MacwireTestCasesSpec.scala', 'w').write(emit('MacwireTestCasesSpec', hdr, shared))
open(out + 'scala-2/hearth/kindlings/di/upstream/MacwireScala2TestCasesSpec.scala', 'w').write(emit('MacwireScala2TestCasesSpec', hdr, s2))
open(out + 'scala-3/hearth/kindlings/di/upstream/MacwireScala3TestCasesSpec.scala', 'w').write(emit('MacwireScala3TestCasesSpec', hdr, s3))
open(out + 'scala-3/hearth/kindlings/di/upstream/MacwireAutowireTestCasesSpec.scala', 'w').write(emit('MacwireAutowireTestCasesSpec', hdr.replace('test-cases/', 'test-cases/autowire/'), autos))
print(len(shared), len(s2), len(s3), len(autos))

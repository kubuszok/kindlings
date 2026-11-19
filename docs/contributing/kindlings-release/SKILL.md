---
name: kindlings-release
description: Preparing and finishing a Kindlings release. Inventory of user-visible changes since the last tag, docs and release notes, and the kubuszok/agent-skills PR that updates the kindlings plugin skills (new features, parity changes, fixed bugs, breaking changes, version bumps) and is merged only after the release is on Maven Central.
---

# Releasing Kindlings

A release is a tag on master. `.github/workflows/release.yml` runs `sbt ci-release`, which publishes to Sonatype /
Maven Central. Sonatype has a publishing quota: when it is exhausted, merge fixes but do not tag until it resets.

The `kindlings` plugin of [kubuszok/agent-skills](https://github.com/kubuszok/agent-skills) describes the **released**
version. Its update lives in one running **draft** PR (branch `kindlings-next`, titled
`kindlings: next release (merge after release)`) that is kept up to date with Kindlings master between releases and
merged only once the release is deployed, so agents never read about behaviour users cannot get yet.

## 0. Between releases: keep the draft PR current

After every Kindlings PR with user-visible changes merges, apply its "## agent-skills" section to the `kindlings-next`
branch (create it from agent-skills master, with the draft PR, if it does not exist) and add one line to the draft PR
body. Mark fixed bugs `fixed after <last-tag>`; do not bump versions yet. Rebase the branch on agent-skills master when
it moves.

## 1. Inventory the changes

```bash
git fetch --tags origin
git log --oneline <last-tag>..origin/master
gh pr list -R kubuszok/kindlings --state closed --search "closed:>=<last-tag-date>" --limit 200
gh issue list -R kubuszok/kindlings --state closed --search "closed:>=<last-tag-date>"
```

PRs merged with `git signed-merge` show as *closed*, not *merged*: use the git log as the source of truth. Each PR's
"agent-skills" section (required by AGENTS.md) says what the skills need.

Classify every user-visible change. Internal changes (refactors, tests, CI, maintainer docs) need nothing.

| Change | User guide | `kindlings` skills |
|---|---|---|
| New module | new page, `index.md`, `mkdocs.yml` nav | new `kindlings-<module>` skill, plugin `README.md` table, `kindlings-getting-started` module map |
| New API, config option, setting key, supported type | module page | `kindlings-<module>` (and `kindlings-troubleshooting` for settings) |
| Parity change (a difference from upstream added, removed or changed) | module page, `feature-parity.md` | `kindlings-<module>`, `kindlings-migration/differences.md` |
| Behaviour, wire-format or binary-compatibility change | module page, release notes "Breaking changes" | `kindlings-migration/upgrading.md`, `kindlings-<module>` |
| Fixed bug | — (remove workarounds from the docs) | `kindlings-troubleshooting`: mark fixed in `<version>`, drop workarounds that are no longer needed |
| New known gotcha | FAQ or module page | "Gotchas" of the matching skill |
| Changed Hearth version | — | `kindlings-custom-integration` (and check the `hearth` plugin is at that Hearth version) |

## 2. Docs and release notes

- Every row above has its user-guide change merged on master before tagging.
- Draft the GitHub release notes: highlights, breaking changes with replacements, fixed issues. Remove every
  maintainer placeholder before publishing.

## 3. The agent-skills PR (before the release)

In a clone of kubuszok/agent-skills (do not switch branches in a checkout another session uses — use a worktree):

1. Check out the `kindlings-next` branch of the running draft PR (section 0); create it from master only if missing.
2. Check every row of the step 1 inventory is applied; apply anything missing. Verify each API, key and default
   against the Kindlings source at the release commit. Replace `fixed after <last-tag>` with `fixed in <version>`.
3. Bump `metadata.library-version` to `<version>` in every `plugins/kindlings/skills/*/SKILL.md`, the plugin `version`
   in `plugins/kindlings/.claude-plugin/plugin.json` and `.cursor-plugin/plugin.json`, the version and docs links in
   `plugins/kindlings/README.md`, and versioned docs links (`/en/<old>/` → `/en/<version>/`).
4. `python3 scripts/validate.py`.
5. Retitle the draft PR `kindlings <version> (merge after release)`. The body is the classified list from step 1: one
   line per user-visible change with its Kindlings PR or issue and the skill section updated. Every new feature,
   parity change and fixed bug has a line; write "no skill change: <reason>" rather than omitting one.

## 4. Release

Tag and push (`git tag <version> && git push origin <version>`), only when the Sonatype quota allows it. Watch the
release workflow.

## 5. After the release is deployed

1. Check the artifacts resolve from Maven Central, e.g.
   `cs resolve com.kubuszok:kindlings-circe-derivation_3:<version>` (it can take a while after the workflow finishes).
2. Publish the GitHub release notes.
3. Rebase the agent-skills PR on master if needed, mark it ready, wait for its CI (`validate`), and merge it with
   `git signed-merge <pr> --repo kubuszok/agent-skills`. The next Kindlings PR with skill changes opens a new
   `kindlings-next` draft (section 0).

import os
import re

def define_env(env):

    kindlings_version_string = ''
    """
    Start by attempting to read git describe --tags
    """
    if not kindlings_version_string:
        try:
            pipe = os.popen('git describe --tags')
            kindlings_version_string = pipe.read().replace("\n", '').strip()
            pipe.close()
        except Exception:
            kindlings_version_string = ''
    """
    If that fails then fallback on CI_LATEST_TAG env var we passed manually to Docker image
    """
    if not kindlings_version_string:
        try:
            kindlings_version_string = env.conf['extra']['local']['tag']
        except KeyError:
            kindlings_version_string = ''
    """
    Finally fallback on something :/
    """
    if not kindlings_version_string:
        kindlings_version_string = 'kindlings_version'
    """
    If git describe tells us that this is NOT a git tag but git tag + some offset, we need to add -SNAPSHOT to match sbt
    """
    if re.compile('.+-[0-9]+-g[0-9a-z]{8}').match(kindlings_version_string):
        kindlings_version_string = kindlings_version_string[0:-1] + '-SNAPSHOT'
    elif re.compile('.+-[0-9]+-[0-9a-z]{8}').match(kindlings_version_string):
        kindlings_version_string = kindlings_version_string + '-SNAPSHOT'

    @env.macro
    def kindlings_version():
        return kindlings_version_string

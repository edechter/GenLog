from starcluster.clustersetup import ClusterSetup
from starcluster.logger import log

class SwiplDevel(ClusterSetup):
     def __init__(self):
          log.debug('Installing swipl-devel')

     def run(self, nodes, master, user, user_shell, volumes):
          for node in nodes:
               log.info("Installing swipl-devel on %s" % (node.alias))
               node.ssh.execute('apt-add-repository ppa:swi-prolog/devel')
               node.ssh.execute('apt-get update')
               node.ssh.execute('apt-get -y install %s' % self.pkg_to_install)

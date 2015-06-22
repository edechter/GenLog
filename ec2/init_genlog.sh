#!/bin/bash -x
# init_genlog.sh
# 
# ----------------------------------------------------------------------

# set up user-data logger
exec > >(tee /var/log/user-data.log|logger -t user-data -s 2>/dev/console)

# pull most recent changes as user edechter
# -i: login shell
# -c: bash flag to read command
sudo -i -u edechter bash -c <<'EOF'
# HACK: fake being an interactive script so that ~/.bashrc executes
export PS1=1

# source .bashrc
source $HOME/.bashrc

echo "Pulling from GenLog repo..."
cd $GENLOG_ROOT
git fetch --all
git reset --hard origin/master
cd -
echo "Done."

echo "Running run_genlog.sh..."
cd $GENLOG_ROOT/ec2
./run_genlog.sh
cd -
echo "Done."

EOF

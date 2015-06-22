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
echo "Pulling from GenLog repo..."
cd $GENLOG_ROOT
git pull origin master
cd -
echo "Done."

echo "Running run_genlog.sh..."
CMD=$GENLOG_ROOT/ec2/run_genlog.sh
echo $CMD
$CMD
echo "Done."

EOF


launch_ec2.sh:  

- launches an ec2 instance using my account. 

- takes the AWS keys from the current environment (those are located in my .profile), so that I can put the scripts on github and not have to publish my secret AWS keys. 

- logs: 
  - user-data log is on the ec2 instance at /var/logs/user-data.log
  - system log is on the ec2 instance at /var/logs/syslog.log
 
- to view the logs from local machine, get the DNS from the ec2 console and type
>> ssh -i ~/Dropbox/ec2.pem ubuntu@<DNS> tail -F /var/logs/*.log

TODO: 
- Add email notification when job completes.

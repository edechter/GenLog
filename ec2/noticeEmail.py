#!/usr/bin/env python
# encoding: utf-8
 
import smtplib
from datetime import datetime
import sys, os
 
 
def noticeEMail(usr, psw, fromaddr, toaddr, subject, msg):
    """
    Sends an email message through GMail once the script is completed.  
    Developed to be used with AWS so that instances can be terminated 
    once a long job is done. Only works for those with GMail accounts.
    
    usr : the GMail username, as a string

    psw : the GMail password, as a string 
    
    fromaddr : the email address the message will be from, as a string
    
    toaddr : a email address, or a list of addresses, to send the 
             message to
    """
 


    # Initialize SMTP server
    server=smtplib.SMTP('smtp.gmail.com:587')
    server.starttls()

    res = server.login(usr,psw)
    
    # Send email
    senddate=datetime.strftime(datetime.now(), '%Y-%m-%d')
    m="Date: %s\r\nFrom: %s\r\nTo: %s\r\nSubject: %s\r\nX-Mailer: My-Mail\r\n\r\n" \
        % (senddate, fromaddr, toaddr, subject)

    m = m + msg
    res = server.sendmail(fromaddr, toaddr, m)
    print res
    server.quit()
    return m
 
 
if __name__ == '__main__':
    usr = os.getenv('GMAIL_USER')
    psw = os.getenv('GMAIL_PASSWORD')
    fromaddr = "%s@gmail.com" % usr
    toaddr = sys.argv[1]
    subj    = sys.argv[2]
    msg = sys.argv[3]
    
 
    # Send notification email
    out = noticeEMail(usr, psw, fromaddr, toaddr, subj, msg)
    print out

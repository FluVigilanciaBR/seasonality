# coding:utf8
from __future__ import print_function

import os.path

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from methods.settings import EMAIL
import base64


# If modifying these scopes, delete the file token.json.
SCOPES = ['https://www.googleapis.com/auth/gmail.compose']
mail_dict = {
    'subject': "InfoGripe Updater -- test",
    'email_body': """
    This is an automated message from InfoGripe Updater.
    System's token configuration update.

    All the best,
    InfoGripe Updater Monitor. 
    """,
    **EMAIL
}


def main():
    """Shows basic usage of the Gmail API.
    Lists the user's Gmail labels.
    """
    creds = None
    # The file token.json stores the user's access and refresh tokens, and is
    # created automatically when the authorization flow completes for the first
    # time.
    if os.path.exists('token.json'):
        creds = Credentials.from_authorized_user_file('token.json', SCOPES)
    # If there are no (valid) credentials available, let the user log in.
    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            flow = InstalledAppFlow.from_client_secrets_file(
                'credentials.json', SCOPES)
            creds = flow.run_local_server(port=0)
        # Save the credentials for the next run
        with open('token.json', 'w') as token:
            token.write(creds.to_json())

    try:
        # Call the Gmail API
        service = build('gmail', 'v1', credentials=creds)
        user_id = '%(NAME)s <%(USER)s>' % mail_dict
        email_msg = MIMEMultipart()
        email_msg['From'] = user_id
        email_msg['To'] = mail_dict['TO']
        email_msg['Subject'] = mail_dict['subject']
        body = MIMEText(mail_dict['email_body'], 'plain')
        email_msg.attach(body)
        message = {'raw': base64.urlsafe_b64encode(email_msg.as_string().encode()).decode()}
        user_id = '%(USER)s' % mail_dict
        message = (service.users().messages().send(userId=user_id, body=message).execute())

    except HttpError as error:
        # TODO(developer) - Handle errors from gmail API.
        print(f'An error occurred: {error}')


if __name__ == '__main__':
    main()
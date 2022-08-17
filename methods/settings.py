import os
import yaml
import pandas as pd

home_path = os.path.expanduser("~")
settings_path = os.path.join(home_path, '.seasonality.yaml')
mailing_list = pd.read_csv(os.path.join(home_path, '.InfoGripe.mailing_list'))

EMAIL = {
    'NAME': None,
    'USER': None,
    'PASSWORD': None,
    'TO': None,
}
EMAILFIOCRUZ = {
    'NAME': None,
    'USER': None,
    'PASSWORD': None,
    'TO': None,
}
SERVER = {
    'USER': None,
    'HOST': None
}
REPORT = {
    'NAME': None,
    'USER': None,
    'PASSWORD': None,
}
SERVERNEW = {
    'USER': None,
    'HOST': None
}

if not os.path.exists(settings_path):
    settings_yaml = {
        'EMAIL': {
            'NAME': '<SENDER_NAME_HERE>',
            'USER': '<SENDER_EMAIL_ADDRESS_HERE>',
            'PASSWORD': '<SENDER_EMAIL_PASSWORD_HERE>',
            'TO': '<EMAIL_RECIPIENT_HERE>'
        },
        'SERVER': {
            'USER': '<SERVER_USER_HERE>',
            'HOST': '<SERVER_HOST_HERE>',
        }
    }

    with open(os.path.join(settings_path), 'w') as f:
        yaml.dump(settings_yaml, f, default_flow_style=False)

    raise Exception('Please configure your settings file (%s)' % settings_path)

with open(os.path.join(settings_path), 'r') as f:
    globals().update(yaml.safe_load(f))

REPORT['CCO'] = ', '.join(mailing_list.lista_de_emails.values)

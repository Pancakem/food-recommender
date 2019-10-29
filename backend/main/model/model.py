import jwt
import uuid
import datetime
from main import db, bcrypt, app

class BadToken(db.Model):

    __tablename__ = "bad_token"

    id = db.Column(db.Integer, autoincrement=True, primary_key=True)
    token = db.Column(db.String, unique=True)
    blacklisted_on = db.Column(db.DateTime, nullable=False)

    def __init__(self, token):
        self.token = token
        self.blacklisted_on = datetime.datetime.now()
    
    def __repr__(self):
        return f'<token: {self.token}>'
    
    @staticmethod
    def check_token(auth_token):
        res = BadToken.query.filter_by(token=str(auth_token)).first()
        if res:
            return True
        else:
            return False

class User(db.Model):
    """ User Model for storing user related details """
    __tablename__ = "users"

    id = db.Column(db.String, primary_key=True)
    fullname = db.Column(db.String(255), nullable=False)
    email = db.Column(db.String(255), unique=True, nullable=False)
    age= db.Column(db.Integer, nullable=False)
    password = db.Column(db.String(255), nullable=False)
    registered_on = db.Column(db.DateTime, nullable=False)
    admin = db.Column(db.Boolean, nullable=False, default=False)

    def __init__(self, fullname, email, age, password, admin=False):
        self.id = str(uuid.uuid4())
        self.fullname = fullname
        self.email = email
        self.age = age
        self.password = bcrypt.generate_password_hash(
            password, app.config.get('BCRYPT_LOG_ROUNDS')
        ).decode()
        self.registered_on = datetime.datetime.now()
        self.admin = admin
    
    def encode_auth_token(self, user_id):
        """
        Generates the Auth Token
        :return: string
        """
        try:
            payload = {
                'exp': datetime.datetime.utcnow() + datetime.timedelta(days=21, seconds=5),
                'iat': datetime.datetime.utcnow(),
                'sub': user_id
            }
            return jwt.encode(
                payload,
                app.config.get('SECRET_KEY'),
                algorithm='HS256'
            )
        except Exception as e:
            return e

    @staticmethod
    def decode_auth_token(auth_token):
        """
        Decodes the auth token
        :param auth_token:
        :return: integer|string
        """
        try:
            payload = jwt.decode(auth_token, app.config.get('SECRET_KEY'))
            is_blacklisted_token = BadToken.check_token(auth_token)
            if is_blacklisted_token:
                return 'Token is bad. Please log in again.'
            else:
                return payload['sub']
        except jwt.ExpiredSignatureError:
            return 'Signature expired. Please log in again.'
        except jwt.InvalidTokenError:
            return 'Invalid token. Please log in again.'

class Settings(db.Model):
     
    __tablename__ = "settings"

    id = db.Column(db.String(255), primary_key=True)
    preference = db.Column(db.String(255))
    protein_intake = db.Column(db.Float)
    carb_intake = db.Column(db.Float)
    fat_intake = db.Column(db.Float)

    def __init__(self, id, preference, protein_intake, carb_intake, fat_intake):
        self.id = id
        self.preference = preference
        self.protein_intake = protein_intake
        self.carb_intake = carb_intake
        self.fat_intake = fat_intake


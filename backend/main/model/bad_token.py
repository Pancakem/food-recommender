import datetime
from main import db

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
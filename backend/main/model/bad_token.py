from .. import db

class BadToken(db.Model):

    __tablename__ = "bad_token"

    id = db.Column(db.Integer, autoincrement=True, primary_key=True)
    token = db.Column(db.String, unique=True)

    def __init__(self, token):
        self.token = token
    
    @staticmethod
	def check_token(auth_token):
		res = BadToken.query.filter_by(token=str(auth_token)).first()

		if res:
			return True
		else:
			return False
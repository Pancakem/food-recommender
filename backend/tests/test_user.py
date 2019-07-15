import unittest
import json

from ..main import db
from ..main.model import User


class TestUserModel(BaseTestCase):

    def test_encode_auth_token(self):
        user = User(
            email='test@example.com',
            password="testcase"
        )

        db.session.add(user)
        db.session.commit()

        auth_token = user.encode_auth_token(user.id)
        self.assertTrue(isinstance(auth_token, bytes))
        self.assertTrue(User.decode_auth_token(auth_token) == 1)

if __name__ == "__main__":
    unittest.main()
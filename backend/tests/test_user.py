import unittest
import json

from tests.base import BaseTestCase
import main.model.user



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
    
    def test_user_status(self):
        """ Test for user status """
        with self.client:
            resp_register = self.client.post(
                '/auth/register',
                data=json.dumps(dict(
                    email='malea@gmail.com',
                    password='123456'
                )),
                content_type='application/json'
            )
            response = self.client.get(
                '/auth/status',
                headers=dict(
                    Authorization='Bearer ' + json.loads(
                        resp_register.data.decode()
                    )['auth_token']
                )
            )
            data = json.loads(response.data.decode())
            self.assertTrue(data['status'] == 'success')
            self.assertTrue(data['data'] is not None)
            self.assertTrue(data['data']['email'] == 'malea@gmail.com')
            self.assertTrue(data['data']['admin'] is 'true' or 'false')
            self.assertEqual(response.status_code, 200)
    
    def test_decode_auth_token(self):
        user = User(
            email='test@test.com',
            password='test'
        )
        db.session.add(user)
        db.session.commit()
        auth_token = user.encode_auth_token(user.id)
        self.assertTrue(isinstance(auth_token, bytes))
        self.assertTrue(User.decode_auth_token(
            auth_token.decode("utf-8") ) == 1)

if __name__ == "__main__":
    unittest.main()
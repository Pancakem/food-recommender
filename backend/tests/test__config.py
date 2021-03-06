from flask_testing import TestCase
from main import app 

class TestDevelopmentConfig(TestCase):
    def create_app(self):
        app.config.from_object('main.config.DevelopmentConfig')
        return app
    
    def test_app_is_development(self):
        self.assertFalse(app.config['SECRET_KEY'] is '')
        self.assertTrue(app.config['DEBUG'] is True)
        self.assertFalse(current_app is None)
        self.assertTrue(
            app.config['SQLALCHEMY_DATABASE_URI'] == ''
        )

class TestTestingConfig(TestCase):
    def create_app(self):
        app.config.from_object('')
        return app
    
    def test_app_is_testing(self):
        self.assertFalse(app.config['SECRET_KEY'] is '')
        self.assertTrue(app.config['DEBUG'])
        self.assertTrue(
            app.config['SQLALCHEMY_DATABASE_URI'] == ''
        )
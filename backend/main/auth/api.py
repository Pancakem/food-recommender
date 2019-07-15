from flask import Blueprint
from main.auth.register import RegisterAPI
from main.auth.login import LoginAPI
from main.auth.status import UserAPI
from main.auth.logout import LogoutAPI

auth_blueprint = Blueprint('auth', __name__)

# define the API resources
registration_view = RegisterAPI.as_view('register_api')
user_view = UserAPI.as_view('user_api')
login_view = LoginAPI.as_view('login_api')
logout_view = LogoutAPI.as_view('logout_api')

# add Rules for API Endpoints
auth_blueprint.add_url_rule(
    '/auth/register',
    view_func=registration_view,
    methods=['POST']
)

auth_blueprint.add_url_rule(
    '/auth/status',
    view_func=user_view,
    methods=['GET']
)

auth_blueprint.add_url_rule(
    '/auth/login',
    view_func=login_view,
    methods=['POST']
)

auth_blueprint.add_url_rule(
    '/auth/logout',
    view_func=logout_view,
    methods=['POST']
)
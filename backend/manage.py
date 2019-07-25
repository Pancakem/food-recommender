import os
import unittest

from flask_migrate import Migrate, MigrateCommand
from flask_script import Manager

from main import app, db
from main.model import model


app.app_context().push()

manager = Manager(app)

migrate = Migrate(app, db)

manager.add_command('db', MigrateCommand)

@manager.command
def run():
	app.run()

@manager.command
def test():
	""" Runs the unit tests """

	tests = unittest.TestLoader().discover('tests', pattern='test*.py')
	result = unittest.TextTestRunner(verbosity=2).run(tests)

	if result.wasSuccessful():
		return 0
	return 1


if __name__ == '__main__':
	manager.run()
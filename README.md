# create_user_db

create_user_db is a plugin for CouchDB that allows a user to create their own database. This is useful when you're not storing your users inside the `_users` DB and so can't use the [couchperuser](https://github.com/etrepum/couchperuser) plugin which watches for changes on the `_users` DB.

## Installation

Install `rebar` if you don't already have it

    $ brew install rebar

Ensure the `plugins` directory exists, e.g.

    $ mkdir /.../couchdb/1.6.0_1/lib/couchdb/plugins

Clone (download) the repo:

    $ git clone https://github.com/buckett/create_user_db

Move the plugin files:

    $ mv create_user_db /.../couchdb/1.6.0_1/lib/couchdb/plugins

Build the plugin files:

    $ cd /.../couchdb/1.6.0_1/lib/couchdb/plugins/create_user_db
    $ make

Copy default config file to etc folder:

    $ cp /.../couchdb/1.6.0_1/lib/couchdb/plugins/create_user_db/priv/default.d/create_user_db.ini /.../couchdb/1.6.0_1/etc/couchdb/default.d/

Restart couchdb

## Test with Curl

    $ curl http://127.0.0.1:5984/_create_user_db

You should see an error stating you need to login as it' can't create a user DB when there's no user. If you supply some credentials then it should create your database.


    $ curl -u admin http://127.0.0.1:5984/_create_user_db
    Enter host password for user 'admin':
    {"db":"userdb-61646d696e"}

Repeatedly requesting this URL shouldn't cause any problems and will just return the name of the DB the user has.


## Use cases

This is useful when allowing per user databases with the [JWT](https://github.com/softapalvelin/couch_jwt_auth) authentication plugin so that each user can create their own DB.

## Development

    $ make dev

This will run CouchDB in interactive mode with the plugin loaded.


## Credits

Thanks to the [couch_jwt_auth](https://github.com/softapalvelin/couch_jwt_auth), [couchperuser](https://github.com/etrepum/couchperuser) and the [my-first-couchdb-plugin](https://github.com/janl/my-first-couchdb-plugin) for their examples that were invaluable.

[Auth0](https://auth0.com/) is an identity service that supports many identity providers like Google, Facebook, Twitter and so on. Auth0 generates a JWT that can be parsed by this plugin. [Here](https://github.com/softapalvelin/getting-started-todo) is a sample application that uses Auth0 to authenticate a user with CouchDB.

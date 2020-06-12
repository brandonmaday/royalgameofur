import json
from flask import Flask, render_template, request, jsonify
from flask_socketio import SocketIO, emit, join_room, leave_room
from flask_sqlalchemy import SQLAlchemy
from flask_login import (
    LoginManager, UserMixin, current_user, login_user, logout_user
)
from typing import NamedTuple, Union

# MAIN

app = Flask(__name__)
app.config["SECRET_KEY"] = "abc"
app.config["SQLALCHEMY_DATABASE_URI"] = "sqlite:///database.db"
app.config["SQLALCHEMY_TRACK_MODIFICATIONS"] = False
db = SQLAlchemy(app) 
socketio = SocketIO(app)
lm = LoginManager(app)

# MODEL

boards = db.Table("boards",
    db.Column("bid", db.Integer, db.ForeignKey("board.id"), primary_key=True),
    db.Column("pid", db.Integer, db.ForeignKey("player.id"), primary_key=True),
)

class Board(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    label = db.Column(db.Text)
    active = db.Column(db.Boolean)

class Player(db.Model, UserMixin):
    id = db.Column(db.Integer, primary_key=True)
    label = db.Column(db.Text)
    sid = db.Column(db.Text)
    boards = db.relationship("Board", secondary=boards, lazy="joined",
        backref=db.backref("players", lazy="joined")
    )

@lm.user_loader
def load_user(user_id):
    return Player.query.get(user_id)

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/login", methods=["POST",])
def login():
    name = request.get_json()
    p = Player.query.filter(Player.label==name).first()
    if not p:
        p = Player(label=name)
        db.session.add(p)
    db.session.commit()
    login_user(p)
    return jsonify({"ok": True})

@app.route("/logout")
def logout():
    current_user.sid = None
    db.session.commit()
    logout_user()
    return "OK"

def make_msg(*args):
    return {"msg": args}

class PlayerData(NamedTuple):
    label: str
    active: bool
    maybeBoard: Union[None, Board]
    room: str
    sid: str

def player(p):
    active_boards = [b for b in p.boards if b.active]
    maybeBoard = None if len(active_boards) == 0 else active_boards[0]
    return PlayerData(
        p.label,
        p.sid is not None,
        maybeBoard,
        maybeBoard.label if maybeBoard else "LOBBY",
        p.sid
    )

def got_players(room):
    emit(
        "update",
        make_msg(
            GOTPLAYERS, [
                x.label
                for x in [player(y) for y in Player.query.all()]
                if x.room == room and x.active
            ]
        ),
        room=room
    )

def got_games():
    games = []
    for b in Board.query.all():
        if b.active:
            if len(b.players) == 1:
                games.append({
                    "open": True,
                    "label": b.label,
                    "p1": b.players[0].label,
                    "p2": "",
                })
            elif len(b.players) == 2:
                games.append({
                    "open": False,
                    "label": b.label,
                    "p1": b.players[0].label,
                    "p2": b.players[1].label,
                })
            else:
                print("board is strange")
                print(b.players)
    emit("update", make_msg(GOTGAMES, games), room="LOBBY")

# MSG

MULTIJOIN = "MULTIJOIN"
GOTPLAYERS = "GOTPLAYERS" # (List String)
JOINED = "JOINED" # String
LOGOUT = "LOGOUT"
MAKEBOARD = "MAKEBOARD"
JOINBOARD = "JOINBOARD"
QUITGAME = "QUITGAME"
GOTGAMES = "GOTGAMES"

@socketio.on("connect")
def conn():
    print(request.sid)
    if current_user.sid is None:
        current_user.sid = request.sid
        db.session.commit()
        p = player(current_user)
        join_room(p.room)
        emit("update", make_msg(JOINED, p.room))
        got_players(p.room)
        got_games()
    else:
        emit("update", make_msg(MULTIJOIN))

@socketio.on("disconnect")
def disconn():
    if current_user.sid == request.sid:
        current_user.sid = None
        db.session.commit()
        got_players(player(current_user).room)

def joinBoard(room):
    print("room")
    print(room)
    got_games()
    leave_room("LOBBY")
    join_room(room)
    emit("update", make_msg(JOINED, room))
    got_players(room)
    got_players("LOBBY")

@socketio.on("update")
def update(msg):
    if msg[0] == LOGOUT:
        p = player(current_user)
        current_user.sid = None
        db.session.commit()
        got_players(p.room)
        emit("update", make_msg(LOGOUT))
    if msg[0] == MAKEBOARD:
        b = Board(active=True)
        current_user.boards.append(b)
        db.session.commit()
        b.label = f"board{b.id}"
        db.session.commit()
        print("blabelmake")
        print(b.label)
        joinBoard(b.label)
    if msg[0] == QUITGAME:
        p = player(current_user)
        p.maybeBoard.active = False
        db.session.commit()
        for boot in p.maybeBoard.players:
            leave_room(p.room, sid=boot.sid)
            join_room("LOBBY", sid=boot.sid)
            emit("update", make_msg(JOINED, "LOBBY"), room=boot.sid)
        got_players("LOBBY")
        got_games()
    if msg[0] == JOINBOARD:
        b = Board.query.filter(Board.label==msg[1]).first()
        b.players.append(current_user)
        db.session.commit()
        print("blabeljoin")
        print(b.label)
        joinBoard(b.label)


if __name__ == "__main__":
    socketio.run(app)

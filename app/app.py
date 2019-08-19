import random
from flask import Flask, Response, request

COOKIE_NAME = 'group'
GROUP_TEXT = {
    'badge': '''
Some medical treatments require a doctor to insert a plastic tube into a large vein. These treatments can save lives, but they can also lead to deadly infections. A hospital director comes up with an idea to reduce these infections. He decides to give each doctor who performs this procedure a new ID badge with a list of standard safety precautions for the procedure printed on the back. All doctors performing this procedure will then have this list attached to their clothing, so they can look at it while performing the procedure. The director thinks that these new badges might help doctors remember all of the safety steps they were trained to take during the procedure.

How appropriate is the director’s decision?
''',
    'poster': '''
Some medical treatments require a doctor to insert a plastic tube into a large vein. These treatments can save lives, but they can also lead to deadly infections. A hospital director comes up with an idea to reduce these infections. He decides to hang a poster with a list of standard safety precautions for this procedure in all the rooms where it is performed. All doctors performing this procedure will then work in rooms with the poster on the wall, so they can look at it while performing the procedure. The director thinks that these posters might help doctors remember all of the safety steps they were trained to take during the procedure.

How appropriate is the director’s decision?
''',
    'bp_short': '''
Some medical treatments require a doctor to insert a plastic tube into a large vein. These treatments can save lives, but they can also lead to deadly infections. A hospital director comes up with two ideas to reduce these infections. He decides to run an experiment to compare these two ideas by randomly assigning patients to one of two groups. Half of the patients will be treated by a doctor who has received a new ID badge with a list of standard safety precautions for the procedure printed on the back. The other half will be treated in a room with a poster listing the same precautions hanging on the wall. The director thinks that these badges and posters might help doctors remember all of the safety steps they were trained to take during the procedure.

How appropriate is the director’s decision?
''',
    'bp_long': '''
Some medical treatments require a doctor to insert a plastic tube into a large vein. These treatments can save lives, but they can also lead to deadly infections. A hospital director comes up with two ideas to reduce these infections. He decides to run an experiment to test compare two ideas by randomly assigning patients to one of two groups. Half of the patients will be treated by a doctor who has received a new ID badge with a list of standard safety precautions for the procedure printed on the back. The other half will be treated in a room with a poster listing the same precautions hanging on the wall. The director thinks that these badges and posters might help doctors remember all of the safety steps they were trained to take during the procedure. After a year, the director will check which option, badges or posters, is most effective, and make it standard for all patients and doctors throughout the entire hospital.

How appropriate is the director’s decision?
''',
}
app = Flask(__name__)


@app.route('/')
def index():
    group = request.cookies.get(COOKIE_NAME)
    if group not in GROUP_TEXT:
        group = None
    if group is None:
        group = random.choice(list(GROUP_TEXT.keys()))
    response = Response(GROUP_TEXT[group])
    response.set_cookie(COOKIE_NAME, group)
    return response

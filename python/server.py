#!/usr/bin/python3

from flask import Flask, jsonify, request

from graphs import MyGraph

app = Flask(__name__)

graph = MyGraph()
query_count = 0

KNOWN_PROBLEM_NAMES = dict(probatio=3, primus=6, secundus=12, tertius=18, quartus=24, quintus=30)

@app.route('/select', methods=['POST'])
def do_select():
    global graph
    global query_count
    query_count += 1
    rq = request.json
    name = rq['problemName']
    n_verts = KNOWN_PROBLEM_NAMES.get(name, None)
    if n_verts is None:
        n_verts = int(name)
    graph.generate(n_verts)
    return jsonify({'problemName': name})

@app.route('/explore', methods=['POST'])
def do_explore():
    global graph
    global query_count
    rq = request.json
    plans = rq['plans']
    results = []
    for plan in plans:
        gates = [int(g) for g in plan]
        labels = graph.explore(gates)
        results.append(labels)
    rs = {'results': results, 'queryCount': query_count}
    return jsonify(rs)

@app.route('/setup', methods=['POST'])
def do_setup():
    global graph
    rq = request.json
    graph.setup(rq['map']['rooms'], rq['map']['startingRoom'], rq['map']['connections'])
    return jsonify({'result': 'ok'})

@app.route('/guess', methods=['POST'])
def do_guess():
    global graph
    rq = request.json
    errors = graph.guess(rq['map']['rooms'], rq['map']['startingRoom'], rq['map']['connections'])
    rs = {'correct': len(errors) == 0, 'errors': errors}
    return jsonify(rs)

@app.route('/hack', methods=['GET'])
def do_hack():
    global graph
    rs = {}
    rs["id"] = "ID"
    rs["map"] = graph.as_json()
    return jsonify(rs)

if __name__ == "__main__":
    app.run(debug=True)


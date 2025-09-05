#!/usr/bin/python3

from flask import Flask, jsonify, request

from graphs import MyGraph

app = Flask(__name__)

graph = MyGraph()
query_count = 0

@app.route('/select', methods=['POST'])
def do_select():
    global graph
    global query_count
    query_count += 1
    rq = request.json
    name = rq['problemName']
    graph.generate(int(name))
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

if __name__ == "__main__":
    app.run(debug=True)


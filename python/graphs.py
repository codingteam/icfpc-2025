#!/usr/bin/python3

import random
from collections import defaultdict
from graph_tool.all import *

class MyGraph:
    def __init__(self):
        self.graph = None
        self.labels = None
        self.verts = []
        self.gates = None
        self.edge_labels = None

    def generate(self, n_verts):
        self.graph = Graph(directed = True)
        self.labels = self.graph.new_vp("int")
        self.gates = defaultdict(dict)
        self.edge_labels = self.graph.new_ep("int")

        self.verts = []
        for i in range(n_verts):
            v = self.graph.add_vertex()
            label = random.randint(0, 3)
            self.labels[v] = label
            #self.graph.vertex_index[v] = i
            self.verts.append(v)

        for v1_idx, v1 in enumerate(self.verts):
            for src_gate_idx in range(6):
                existing_edge = self.gates[v1_idx].get(src_gate_idx, None)
                if existing_edge is not None:
                    continue
                #print(f"connect [{v1_idx}].{src_gate_idx} to...")
                found = False
                while not found:
                    v2_idx = random.randint(0, n_verts-1)
                    dst_gate_idx = random.randint(0, 5)
                    #print(f"check: [{v2_idx}].{dst_gate_idx}...")
                    existing_edge = self.gates[v2_idx].get(dst_gate_idx, None)
                    #print(f"edge: {existing_edge}")
                    if existing_edge is None:
                        found = True
                        edge = self.graph.add_edge(v1, self.verts[v2_idx])
                        self.gates[v1_idx][src_gate_idx] = edge
                        self.gates[v2_idx][dst_gate_idx] = edge
                        self.edge_labels[edge] = src_gate_idx
                        #print(f"...selected: [{v2_idx}].{dst_gate_idx}")
                        break

    def other_vert(self, edge, vert):
        if edge.source() == vert:
            return edge.target()
        else:
            return edge.source()

    def draw(self, filename=None):
        graph_draw(self.graph, vertex_text = self.labels, edge_text = self.edge_labels, output=filename)

    def explore(self, path):
        idx = 0
        result = [self.labels[self.verts[idx]]]
        for gate_idx in path:
            edge = self.gates[idx][gate_idx]
            next_vert = self.other_vert(edge, self.verts[idx])
            label = self.labels[next_vert]
            result.append(label)
            idx = self.graph.vertex_index[next_vert]
        return result

    def guess(self, rooms, starting_room, connections):
        errors = []
        if len(rooms) != len(self.verts):
            errors.append(f"number of rooms {len(rooms)} is not equal to actual number {len(self.verts)}")
            return errors
        for vert_idx, label in enumerate(rooms):
            if self.labels[vert_idx] != label:
                errors.append(f"Label of room #{vert_idx} label != actual label {self.labels[vert_idx]}")
        if starting_room != 0:
            errors.append(f"Starting room index {starting_room} is not equal to actual 0")
            return errors
        if len(connections) != self.graph.num_edges():
            errors.append(f"Number of edges {len(connections)} is not equal to actual {self.graph.num_edges()}")
            return errors
        for connection in connections:
            f = connection['from']
            t = connection['to']
            v1_idx = f['room']
            v2_idx = t['room']
            gate1_idx = f['door']
            gate2_idx = t['door']
            existing_edge = self.graph.edge(self.verts[v1_idx], self.verts[v2_idx])
            if not existing_edge:
                existing_edge = self.graph.edge(self.verts[v2_idx], self.verts[v1_idx])
            if not existing_edge:
                errors.append(f"Edge between rooms #{v1_idx} and #{v2_idx} does not exist")
            existing_edge = self.gates[v1_idx].get(f['door'], None)
            if self.gates[v2_idx].get(t['door'], None) != existing_edge:
                errors.append(f"There is no edge between room #{v1_idx} door {gate1_idx} and room #{v2_idx} door {gate2_idx}")
        return errors

    def setup(self, rooms, starting_room, connections):
        self.verts = []
        self.graph = Graph(directed = True)
        self.labels = self.graph.new_vp("int")
        self.gates = defaultdict(dict)
        self.edge_labels = self.graph.new_ep("int")

        n_verts = len(rooms)

        for i, label in enumerate(rooms):
            v = self.grpah.add_vertex()
            self.labels[v] = label
            self.verts.append(v)

        for connection in connections:
            f = connection['from']
            t = connection['to']
            v1_idx = f['room']
            v2_idx = t['room']
            gate1_idx = f['door']
            gate2_idx = t['door']
            edge = self.graph.add_edge(self.verts[v1_idx], self.verts[v2_idx])
            self.gates[v1_idx][gate1_idx] = edge
            self.gates[v2_idx][gate2_idx] = edge
            self.edge_labels[edge] = gate1_idx

    def find_gate(self, vert_idx, edge):
        for gate_idx in self.gates[vert_idx]:
            if self.gates[vert_idx][gate_idx] == edge:
                return gate_idx
        return None

    def as_json(self):
        rs = {}
        rs['rooms'] = [self.labels[v] for v in self.verts]
        rs['startingRoom'] = 0
        connections = []
        for edge in self.graph.edges():
            v1_idx = self.graph.vertex_index[edge.source()]
            v2_idx = self.graph.vertex_index[edge.target()]
            gate1_idx = self.find_gate(v1_idx, edge)
            gate2_idx = self.find_gate(v2_idx, edge)
            connection = {'from': {'room': v1_idx, 'door': gate1_idx}, 'to': {'room': v2_idx, 'door': gate2_idx}}
            connections.append(connection)
        rs['connections'] = connections
        return rs

#g = MyGraph()
#g.generate(3)
#g.draw()

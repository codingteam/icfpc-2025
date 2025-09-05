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
        #self.vert_idxs = self.graph.new_vp("int")
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

#g = MyGraph()
#g.generate(3)
#g.draw()

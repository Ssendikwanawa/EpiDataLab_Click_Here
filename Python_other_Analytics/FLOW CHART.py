from graphviz import Digraph

dot = Digraph()

dot.node('A', 'Start')
dot.node('B', 'Process 1')
dot.node('C', 'Decision')
dot.node('D', 'Process 2')
dot.node('E', 'End')

dot.edges(['AB', 'BC', 'CD', 'DE'])
dot.edge('C', 'E', label='No')
dot.edge('C', 'D', label='Yes')

dot.render('flowchart', format='png', cleanup=True)  # Saves flowchart as 'flowchart.png'

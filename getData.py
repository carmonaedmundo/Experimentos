from ubidots import ApiClient
import plotly
import plotly.graph_objs as go

api = ApiClient(token='KZFd2q5tOatgCFlNNmjku9xp0e1mVP')
xVar = api.get_variable('58f986aa76254238893be026')
zVar = api.get_variable('58f986aa762542388a3aca2d')
yVar = api.get_variable('58f986aa762542388835e35c')

xList =  xVar.get_values()
zList =  yVar.get_values()
yList =  zVar.get_values()

x = []
z = []
y = []

current = 0
for current in range(len(xList)):
     x.append( xList[current]['value'])
     z.append( zList[current]['value'])
     y.append( yList[current]['value'])

trace1 = go.Scatter3d(
    x=x,
    y=y,
    z=z,
    mode='markers',
    marker=dict(
        size=12,
        line=dict(
            color='rgba(217, 217, 217, 0.14)',
            width=0.5
        ),
        opacity=0.8
    )
)
data = [trace1]
layout = go.Layout(
    margin=dict(
        l=0,
        r=0,
        b=0,
        t=0
    )
)
fig = go.Figure(data=data, layout=layout)

plotly.offline.plot(fig) 


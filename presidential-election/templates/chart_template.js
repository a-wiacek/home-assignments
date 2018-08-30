google.charts.load('current', {'packages':['corechart']});
google.charts.setOnLoadCallback(drawChart);
function drawChart() {

    var data = google.visualization.arrayToDataTable([
        ['Kandydat', 'Otrzymane głosy']
        {% for x in results %}
        ,['{{ x[0] }}', {{ x[1] }}]
        {% endfor %}
        ]);

    var options = {
        'chartArea': {
            'width': '100%',
            'height': '80%'
        },
        sliceVisibilityThreshold: 0
    };

    var chart = new google.visualization.PieChart(document.getElementById('piechart'));

    chart.draw(data, options);
}

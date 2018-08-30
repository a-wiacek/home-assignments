google.charts.load('current', {'packages':['corechart']});
google.charts.setOnLoadCallback(drawChart);
function drawChart() {

    var data = google.visualization.arrayToDataTable([
        ['Kandydat', 'Otrzymane głosy'],
        ['Dariusz Grabowski', 89002],
        ['Piotr Ikonowicz', 38672],
        ['Jarosław Kalinowski', 1047949],
        ['Janusz Korwin-Mikke', 252499],
        ['Marian Krzaklewski', 2739621],
        ['Aleksander Kwaśniewski', 9485224],
        ['Andrzej Lepper', 537570],
        ['Jan Łopuszański', 139682],
        ['Andrzej Olechowski', 3044141],
        ['Bogdan Pawłowski', 17164],
        ['Lech Wałęsa', 178590],
        ['Tadeusz Wilecki', 28805]
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

google.charts.load('current', {'packages':['geochart'], 'mapsApiKey': 'AIzaSyD-9tSrke72PouQMnMX-a7eZSW0jkFMBWY'});
google.charts.setOnLoadCallback(drawRegionsMap);
function drawRegionsMap() {
    var data = google.visualization.arrayToDataTable([
        ['Województwo', 'Frekwencja'],
        ['mazowieckie', 61.41],
        ['dolnośląskie', 60.60],
        ['lubelskie', 59.04],
        ['kujawsko-pomorskie', 62.08],
        ['warmińsko-mazurskie', 58.13],
        ['lubuskie', 61.02],
        ['podkarpackie', 61.53],
        ['wielkopolskie', 66.54],
        ['podlaskie', 58.49],
        ['pomorskie', 62.83],
        ['zachodniopomorskie', 60.14],
        ['śląskie', 60.84],
        ['świętokrzyskie', 58.85],
        ['opolskie', 51.28],
        ['małopolskie', 62.22],
        ['łódzkie', 61.45]
    ]);

    var formatter = new google.visualization.NumberFormat({suffix: '%'});
    formatter.format(data, 1); // Apply formatter to second column

    var options = {
        region: 'PL',
        resolution: 'provinces',
        legend: {numberFormat: '#\'%\''},
        colorAxis: {colors:['red', 'green']}
    };

    var chart = new google.visualization.GeoChart(document.getElementById('regions_div'));

    chart.draw(data, options);
    google.visualization.events.addListener(chart, 'select', selectHandler);

    function selectHandler(e) {
        var row = chart.getSelection()[0].row;
        var value = data.getValue(row, 0);
        window.location = './' + value + '/index.html';
    }
}

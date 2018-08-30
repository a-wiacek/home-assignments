import csv, jinja2, os, shutil, numpy, io, time, sys
from districts import itod

start = time.time()

data = []
with open('pkw2000.csv', encoding='utf8') as file:
    csvdata = csv.reader(file)
    for row in csvdata:
        row[0] = row[0].lower()
        data.append(row)

columnNames = data.pop(0)
candidates = columnNames[11:22]
voivodeships = set(row[0] for row in data)

# Clear garbage after previous generation
for voivodeship in voivodeships:
    shutil.rmtree('../wybory/' + voivodeship, ignore_errors=True)

# Call with any argument to clear data only
if len(sys.argv) > 1:
    exit(0)

Gfile = open('gmina_template.html')

for voivodeship in voivodeships:
    os.mkdir('../wybory/' + voivodeship)
    voivData = list(filter((lambda x: x[0] == voivodeship), data))
    results = []
    allVotes = sum(int(row[10]) for row in voivData)
    for c in candidates:
        votes = 0
        for row in voivData:
            votes += int(row[columnNames.index(c)])
        results.append([c, str(votes), str('%.2f' % round(100. * votes / allVotes, 2))])
    # Turnout rate = given v.c. / eligible voters ([1]/[0])
    turnout = numpy.array(voivData)[:, 6:11].tolist()
    turnout = [[int(y) for y in x] for x in turnout]
    turnout = numpy.sum(turnout, axis=0).tolist()
    turnout.append(str('%.2f' % round(100. * turnout[1] / turnout[0], 2)))
    districts = list(map(int, list(set(numpy.array(voivData)[:, 1]))))
    districts.sort()
    districts = [[itod[x], x] for x in districts]
    # HTML template
    Vfile = io.open('voivodeship_template.html', mode='r', encoding='utf-8')
    template = jinja2.Template(Vfile.read())
    generatedHtml = template.render(voivodeship=voivodeship, results=results, turnout=turnout, districts=districts)
    with io.open('../wybory/' + voivodeship + '/index.html', mode='w', encoding='utf-8') as file:
        file.write(generatedHtml)
    # JS template
    Cfile = io.open('chart_template.js', mode='r', encoding='utf-8')
    template = jinja2.Template(Cfile.read())
    generatedJS = template.render(results=results)
    with io.open('../wybory/' + voivodeship + '/chart.js', mode='w', encoding='utf-8') as file:
        file.write(generatedJS)
    for districtArray in districts:
        os.mkdir('../wybory/' + voivodeship + '/o' + str(districtArray[1]))
        distrData = list(filter((lambda x: x[1] == str(districtArray[1])), voivData))
        results = []
        allVotes = sum(int(row[10]) for row in distrData)
        for c in candidates:
            votes = 0
            for row in distrData:
                votes += int(row[columnNames.index(c)])
            results.append([c, str(votes), str('%.2f' % round(100. * votes / allVotes, 2))])
        # Turnout rate = given v.c. / eligible voters ([1]/[0])
        turnout = numpy.array(distrData)[:, 6:11].tolist()
        turnout = [[int(y) for y in x] for x in turnout]
        turnout = numpy.sum(turnout, axis=0).tolist()
        turnout.append(str('%.2f' % round(100. * turnout[1] / turnout[0], 2)))
        powiaty = list(set(numpy.array(distrData)[:, 4]))
        powiaty.sort()
        # HTML template
        Dfile = io.open('district_template.html', mode='r', encoding='utf-8')
        template = jinja2.Template(Dfile.read())
        generatedHtml = template.render(district=districtArray, results=results, turnout=turnout, powiaty=powiaty)
        with io.open('../wybory/' + voivodeship + '/o' + str(districtArray[1]) + '/index.html',
                     mode='w', encoding='utf-8') as file:
            file.write(generatedHtml)
        # JS template
        Cfile = io.open('chart_template.js', mode='r', encoding='utf-8')
        template = jinja2.Template(Cfile.read())
        generatedJS = template.render(results=results)
        with io.open('../wybory/' + voivodeship + '/o' + str(districtArray[1]) + '/chart.js',
                     mode='w', encoding='utf-8') as file:
            file.write(generatedJS)
        for powiat in powiaty:
            os.mkdir('../wybory/' + voivodeship + '/o' + str(districtArray[1]) + '/' + powiat)
            powData = list(filter((lambda x: x[4] == powiat), distrData))
            results = []
            allVotes = sum(int(row[10]) for row in powData)
            for c in candidates:
                votes = 0
                for row in powData:
                    votes += int(row[columnNames.index(c)])
                results.append([c, str(votes), str('%.2f' % round(100. * votes / allVotes, 2))])
            # Turnout rate = given v.c. / eligible voters ([1]/[0])
            turnout = numpy.array(powData)[:, 6:11].tolist()
            turnout = [[int(y) for y in x] for x in turnout]
            turnout = numpy.sum(turnout, axis=0).tolist()
            turnout.append(str('%.2f' % round(100. * turnout[1] / turnout[0], 2)))
            gminy = list(numpy.array(powData)[:, 2:4])
            gminy.sort(key=lambda x: x[1])
            # HTML template
            Dfile = io.open('powiat_template.html', mode='r', encoding='utf-8')
            template = jinja2.Template(Dfile.read())
            generatedHtml = template.render(powiat=powiat, results=results, turnout=turnout, gminy=gminy)
            with io.open('../wybory/' + voivodeship + '/o' + str(districtArray[1]) + '/' + powiat + '/index.html',
                         mode='w', encoding='utf-8') as file:
                file.write(generatedHtml)
            # JS template
            Cfile = io.open('chart_template.js', mode='r', encoding='utf-8')
            template = jinja2.Template(Cfile.read())
            generatedJS = template.render(results=results)
            with io.open('../wybory/' + voivodeship + '/o' + str(districtArray[1]) + '/' + powiat + '/chart.js',
                         mode='w', encoding='utf-8') as file:
                file.write(generatedJS)
            for gminaArray in gminy:
                os.mkdir('../wybory/' + voivodeship + '/o' + str(districtArray[1]) + '/' + powiat
                         + '/g' + str(gminaArray[0]))
                gmData = list(filter((lambda x: x[2] == str(gminaArray[0])), powData))
                results = []
                allVotes = sum(int(row[10]) for row in gmData)
                for c in candidates:
                    votes = 0
                    for row in gmData:
                        votes += int(row[columnNames.index(c)])
                    results.append([c, str(votes), str('%.2f' % round(100. * votes / allVotes, 2))])
                # Turnout rate = given v.c. / eligible voters ([1]/[0])
                turnout = numpy.array(gmData)[:, 6:11].tolist()
                turnout = [[int(y) for y in x] for x in turnout]
                turnout = numpy.sum(turnout, axis=0).tolist()
                turnout.append(str('%.2f' % round(100. * turnout[1] / turnout[0], 2)))
                # HTML template
                Gfile = io.open('gmina_template.html', mode='r', encoding='utf-8')
                template = jinja2.Template(Gfile.read())
                generatedHtml = template.render(gmina=gminaArray, results=results, turnout=turnout)
                with io.open('../wybory/' + voivodeship + '/o' + str(districtArray[1]) + '/' + powiat
                             + '/g' + str(gminaArray[0]) + '/index.html', mode='w', encoding='utf-8') as file:
                    file.write(generatedHtml)
                # JS template
                Cfile = io.open('chart_template.js', mode='r', encoding='utf-8')
                template = jinja2.Template(Cfile.read())
                generatedJS = template.render(results=results)
                with io.open('../wybory/' + voivodeship + '/o' + str(districtArray[1]) + '/' + powiat
                             + '/g' + str(gminaArray[0]) + '/chart.js', mode='w', encoding='utf-8') as file:
                    file.write(generatedJS)

print("Program run for " + str('%.2f' % round(time.time() - start)) + "s")

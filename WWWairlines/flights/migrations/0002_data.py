from django.db import migrations, IntegrityError, transaction
from random import randrange, choice, randint, sample
from datetime import timedelta, datetime
import pytz

names_arr = ["Jan", "Zbigniew", "Kamil", "Patryk", "Janusz", "Grzegorz", "Szymon", "Jakub", "Adam", "Dominik",
             "Józef", "Ryszard", "Marcin", "Michał", "Krzysztof", "Tomasz", "Wiktor", "Karol", "Wojciech",
             "Bogdan", "Marek", "Cezary", "Lucjan", "Tadeusz", "Ferdynand", "Marian", "Mirosław", "Jarosław",]
surnames_arr = ["Nowak", "Kowalski", "Wiśniewski", "Zieliński", "Wójcik", "Kowalczyk", "Stonoga", "Lewandowski",
                "Dąbrowski", "Kozłowski", "Jankowski", "Mazur", "Krawczyk", "Norek", "Kozioł", "Kaczmarek",
                "Kaczmarczyk", "Kiepski", "Paździoch", "Zając", "Stępień", "Malinowski", "Jaworski",
                "Pawlak", "Górski", "Baran", "Rutkowski", "Bobieński", "Engel", "Wróblewski", "Zawadzki",
                "Duda", "Komorowski", "Kaczyński", "Ziobro", "Wilk", "Saj", "Niesiołowski", "Palikot"]

# https://stackoverflow.com/questions/553303/generate-a-random-date-between-two-other-dates
def random_date(start, end):
    """
    This function will return a random datetime between two datetime
    objects.
    """
    delta = end - start
    int_delta = (delta.days * 24 * 60 * 60) + delta.seconds
    random_second = randrange(int_delta)
    return start + timedelta(seconds=random_second)


def initialData(apps, schema_editor):
    # Constants
    N_PEOPLE = 250
    N_CREWS = 25
    # These two don't include captain
    N_CREW_SIZE_MIN = 3
    N_CREW_SIZE_MAX = 5
    N_PLANE = 80
    PL_MIN_CAP = 25
    PL_MAX_CAP = 250
    N_AIRPORT = 125
    A_BUCKET_SIZE = 100  # Needs to be divisble by 2
    N_FLIGHT = 80
    N_MIN_FLIGHT_TIME = 7
    N_MAX_FLIGHT_TIME = 11
    # People
    Person = apps.get_model('flights', 'Person')
    Person.objects.all().delete()
    for i in range(N_PEOPLE):
        name = choice(names_arr) + ' ' + choice(surnames_arr)
        Person.objects.get_or_create(name=name)
    # Employees and Crews
    Employee = apps.get_model('flights', 'Employee')
    Employee.objects.all().delete()
    Crew = apps.get_model('flights', 'Crew')
    Crew.objects.all().delete()
    for i in range(N_CREWS):
        name = choice(names_arr) + ' ' + choice(surnames_arr)
        (captain, _) = Employee.objects.get_or_create(name=name)
        if captain.captainPermissions:
            continue
        # captain.captainPermissions = True --> To nie działa, jak to poprawić?
        Employee.objects.filter(name=captain.name).delete()
        (captain, _) = Employee.objects.get_or_create(name=name, captainPermissions=True)
        crew_size = randint(N_CREW_SIZE_MIN, N_CREW_SIZE_MAX)
        (crew, _) = Crew.objects.get_or_create(captain=captain)
        for j in range(crew_size):
            name = choice(names_arr) + ' ' + choice(surnames_arr)
            (employee, _) = Employee.objects.get_or_create(name=name)
            crew.members.add(employee)
    # Planes
    Plane = apps.get_model('flights', 'Plane')
    Plane.objects.all().delete()
    for i in range(N_PLANE):
        name = "Samolot nr " + str(i)
        Plane.objects.get_or_create(name=name, capacity=randint(PL_MIN_CAP, PL_MAX_CAP))
    # Airports
    Airport = apps.get_model('flights', 'Airport')
    Airport.objects.all().delete()
    for i in range(N_AIRPORT):
        name = "Lotnisko nr " + str(i)
        Airport.objects.get_or_create(name=name)
    # Flights
    Flight = apps.get_model('flights', 'Flight')
    Flight.objects.all().delete()
    dep_date = pytz.utc.localize(datetime.now())
    hour = timedelta(hours=1)
    random_bucket = []
    for i in range(N_FLIGHT):
        for plane in Plane.objects.all():
            if not random_bucket:
                random_bucket = sample(range(0, N_AIRPORT - 1), A_BUCKET_SIZE)
            dep_airport = Airport.objects.all()[random_bucket.pop()]
            arr_airport = Airport.objects.all()[random_bucket.pop()]
            arr_date = dep_date + timedelta(hours=randint(N_MIN_FLIGHT_TIME, N_MAX_FLIGHT_TIME))
            crew = choice(Crew.objects.all())
            Flight.objects.get_or_create(
                departureAirport=dep_airport,
                departureDate=dep_date,
                arrivalAirport=arr_airport,
                arrivalDate=arr_date,
                plane=plane,
                crew=crew
            )
            dep_date += hour


class Migration(migrations.Migration):

    dependencies = [
        ('flights', '0001_initial'),
    ]

    operations = [
        migrations.RunPython(initialData)
    ]

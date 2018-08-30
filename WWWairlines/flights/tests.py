import ast

from django.contrib.auth.models import User
from django.contrib.staticfiles.testing import StaticLiveServerTestCase
from django.test import TestCase

from flights.models import Plane, Airport, Employee, Crew, Flight, Person
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import datetime
from time import sleep


class DjangoTest(TestCase):
    def setUp(self):
        User.objects.create_user(username='login', password='qwerty')
        planeA = Plane.objects.create(name="Nimbus 2000", capacity=5)
        Plane.objects.create(name="Firebolt", capacity=5)
        airportX = Airport.objects.create(name="Hogwart")
        airportY = Airport.objects.create(name="London")
        cpt1 = Employee.objects.create(name="Harry Potter", captainPermissions=True)
        cpt2 = Employee.objects.create(name="Draco Malfoy", captainPermissions=True)
        emp1 = Employee.objects.create(name="Ron Weasley")
        emp2 = Employee.objects.create(name="Hermiona Granger")
        crew1 = Crew.objects.create(captain=cpt1)
        Crew.objects.create(captain=cpt2)
        crew1.members.add(emp1)
        crew1.members.add(emp2)
        now = datetime.datetime.now()
        Flight.objects.create(
            departureAirport=airportX,
            arrivalAirport=airportY,
            departureDate=now,
            arrivalDate=now + datetime.timedelta(hours=5),
            plane=planeA,
            crew=crew1
        )

    def test__getCrew(self):
        response = ast.literal_eval(self.client.get('/ajax/getCrewMembers/', {'captain': "Harry Potter"})
                                    .content.decode('utf-8'))['members']
        expectedAnswer = ["Ron Weasley", "Hermiona Granger"]
        self.assertEquals(set(response), set(expectedAnswer))

    def test__addCrew(self):
        (_, created) = Flight.objects.get_or_create(
            departureAirport=Airport.objects.get(name="Hogwart"),
            arrivalAirport=Airport.objects.get(name="London"),
            departureDate=datetime.datetime.now() + datetime.timedelta(hours=8),
            arrivalDate=datetime.datetime.now() + datetime.timedelta(hours=11),
            plane=Plane.objects.get(name="Nimbus 2000"),
            crew=Crew.objects.get(captain="Harry Potter")
        )
        self.assertTrue(created)

    def test__addAnotherCrew(self):
        flight = Flight.objects.create(
            departureAirport=Airport.objects.get(name="Hogwart"),
            arrivalAirport=Airport.objects.get(name="London"),
            departureDate=datetime.datetime.now() + datetime.timedelta(hours=4),
            arrivalDate=datetime.datetime.now() + datetime.timedelta(hours=7),
            plane=Plane.objects.get(name="Nimbus 2000"),
            crew=Crew.objects.get(captain="Draco Malfoy")
        )
        response = self.client.post('/ajax/changeCrewAssignment/',
            {'username': "login", 'password': 'qwerty', 'flightId': flight.id, 'newCaptain': "Harry Potter"})\
            .content.decode('utf-8')
        self.assertEquals(response, "Załoga jest już zajęta w tym czasie.")


class SeleniumTest(StaticLiveServerTestCase):
    def setUp(self):
        User.objects.create_user(username='login', password='qwerty', is_staff=True)
        planeA = Plane.objects.create(name="Nimbus 2000", capacity=5)
        planeB = Plane.objects.create(name="Firebolt", capacity=5)
        airportX = Airport.objects.create(name="Hogwart")
        airportY = Airport.objects.create(name="London")
        cpt1 = Employee.objects.create(name="Harry Potter", captainPermissions=True)
        cpt2 = Employee.objects.create(name="Draco Malfoy", captainPermissions=True)
        emp1 = Employee.objects.create(name="Ron Weasley")
        emp2 = Employee.objects.create(name="Hermiona Granger")
        crew1 = Crew.objects.create(captain=cpt1)
        crew2 = Crew.objects.create(captain=cpt2)
        crew1.members.add(emp1)
        crew1.members.add(emp2)
        now = datetime.datetime.now()
        Flight.objects.create(
            departureAirport=airportX,
            arrivalAirport=airportY,
            departureDate=now + datetime.timedelta(hours=3),
            arrivalDate=now + datetime.timedelta(hours=7),
            plane=planeA,
            crew=crew1
        )
        Flight.objects.create(
            departureAirport=airportX,
            arrivalAirport=airportY,
            departureDate=now + datetime.timedelta(hours=4),
            arrivalDate=now + datetime.timedelta(hours=7),
            plane=planeB,
            crew=crew2
        )
        Person.objects.create(name="Krzysztof Ibisz")

    def test__bigTest(self):
        browser = webdriver.Firefox()
        browser.get(self.live_server_url)
        # Logowanie
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "login")))
        browser.find_element_by_css_selector('#login').click()
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "id_username")))
        browser.find_element_by_css_selector('#id_username').send_keys('login')
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "id_password")))
        browser.find_element_by_css_selector('#id_password').send_keys('qwerty')
        browser.find_element_by_css_selector('input[type="submit"]').click()
        browser.get(self.live_server_url)
        # Dodanie pasażera
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "flight1")))
        browser.find_element_by_css_selector('#flight1').click()
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "id_person")))
        for option in browser.find_element_by_css_selector('#id_person').find_elements_by_tag_name('option'):
            if option.text == "Krzysztof Ibisz":
                option.click()
                break
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "id_tickets")))
        browser.find_element_by_css_selector('#id_tickets').send_keys('3')
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "id_confirm")))
        browser.find_element_by_css_selector('#id_confirm').click()
        browser.refresh()
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.CSS_SELECTOR, "table")))
        self.assertEquals(browser.find_element_by_css_selector('table tbody tr:nth-child(2)').text,
                          "Krzysztof Ibisz 3")
        browser.get(self.live_server_url)
        # Edycja załóg
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "edit-crews")))
        browser.find_element_by_css_selector('#edit-crews').click()
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "input-username")))
        browser.find_element_by_css_selector('#input-username').send_keys('login')
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "input-password")))
        browser.find_element_by_css_selector('#input-password').send_keys('qwerty')
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "login-button")))
        browser.find_element_by_css_selector('#login-button').click()
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "select-date")))
        # https://github.com/mozilla/geckodriver/issues/1070
        date = (datetime.datetime.now() + datetime.timedelta(hours=3)).strftime("%Y-%m-%d")
        date_field = browser.find_element_by_css_selector('#select-date')
        date_field.click()
        date_field.send_keys(date)
        WebDriverWait(browser, 10).until(EC.presence_of_element_located((By.ID, "flight1")))
        for option in browser.find_element_by_css_selector('#flight1').find_elements_by_tag_name('option'):
            if option.text == "Draco Malfoy":
                option.click()
                break
        WebDriverWait(browser, 10).until(EC.alert_is_present())
        self.assertEquals(browser.switch_to.alert.text,
                          "Nie udało się zmienić załogi. W tym czasie ma ona już jakiś lot do odbycia.")

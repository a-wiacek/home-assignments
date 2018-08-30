from django.contrib.auth import authenticate
from django.http import HttpResponseForbidden, HttpResponse, JsonResponse
from django.shortcuts import render, get_object_or_404, redirect
from django.db.models import Sum
from django.db import transaction, IntegrityError
from django.views.decorators.csrf import csrf_exempt
from django.views.decorators.http import require_POST, require_GET
from .models import *
from .forms import BookingForm, DataFilterForm
import datetime
import pytz
from django.template.defaulttags import register


@register.filter
def get_item(dictionary, key):
    value = dictionary.get(key)
    if value:
        return value
    else:
        return 0


def concat(array):
    return array[0] + " " + array[1]


def main(request):
    flights = Flight.objects.all().order_by('departureDate')
    if request.method == 'GET':
        filterByData = DataFilterForm(request.GET)
        filterByData.full_clean()
        try:
            if filterByData.is_valid():
                depMinDate = filterByData['depMinDate'].value()
                if depMinDate[0]:
                    flights = flights.filter(departureDate__gt=concat(depMinDate))
                depMaxDate = filterByData['depMaxDate'].value()
                if depMaxDate[0]:
                    flights = flights.filter(departureDate__lte=concat(depMaxDate))
                arrMinDate = filterByData['arrMinDate'].value()
                if arrMinDate[0]:
                    flights = flights.filter(arrivalDate__gt=concat(arrMinDate))
                arrMaxDate = filterByData['arrMaxDate'].value()
                if arrMaxDate[0]:
                    flights = flights.filter(arrivalDate__lte=concat(arrMaxDate))
        except ValidationError:
            pass
    bookings = list(Booking.objects.values('flight').annotate(passengers=Sum('tickets')))
    bookings_dict = dict((d['flight'], d['passengers']) for d in bookings)
    return render(request, 'index.html', locals())


def flight(request, flightID):
    flight = get_object_or_404(Flight, id=flightID)
    passengers = Booking.objects.filter(flight=flight)
    date_now = pytz.UTC.localize(datetime.datetime.now())
    status = request.GET.get('status')
    passengersCount = passengers.aggregate(Sum('tickets'))['tickets__sum']
    if passengersCount is None:
        passengersCount = 0
    if request.user.is_authenticated:
        if request.method == 'POST':
            addBookingForm = BookingForm(request.POST)
            addBookingForm.full_clean()
            if addBookingForm.is_valid():
                try:
                    with transaction.atomic():
                        tickets = int(addBookingForm['tickets'].value())
                        if passengersCount + tickets <= flight.plane.capacity:
                            booking = addBookingForm.save(commit=False)
                            booking.flight = flight
                            booking.save()
                            response = redirect('flight', flightID)
                            response['Location'] += '?status=ok'
                            return response
                        else:
                            response = redirect('flight', flightID)
                            response['Location'] += '?status=err'
                            return response
                except IntegrityError:
                    response = redirect('flight', flightID)
                    response['Location'] += '?status=err'
                    return response
        else:
            addBookingForm = BookingForm()
    return render(request, 'flight.html', locals())


@require_POST
@csrf_exempt
def checkLoginStatus(request):
    user = authenticate(username=request.POST['username'], password=request.POST['password'])
    if user is None:
        return HttpResponseForbidden("Użytkownik niezalogowany")
    else:
        return HttpResponse()


@require_GET
@csrf_exempt
def getFlightsOnDate(request):
    date_string = request.GET['date']
    date_format = '%Y-%m-%d'
    start_date = datetime.datetime.strptime(date_string, date_format)
    end_date = start_date + datetime.timedelta(days=1)
    flights = Flight.objects.filter(departureDate__range=(start_date, end_date)).values()
    captains = Employee.objects.filter(captainPermissions=True).values_list('name', flat=True)
    return JsonResponse({'flights': list(flights), 'captains': list(captains)})


@require_POST
@csrf_exempt
def changeCrewAssignment(request):
    user = authenticate(username=request.POST['username'], password=request.POST['password'])
    if user is None:
        return HttpResponseForbidden("Użytkownik niezalogowany.")
    flight_to_change = get_object_or_404(Flight, id=request.POST['flightId'])
    old_crew = flight_to_change.crew
    new_crew = get_object_or_404(Crew, captain=request.POST['newCaptain'])
    try:
        flight_to_change.crew = new_crew
        with transaction.atomic():  # POPRAWKA
            flight_to_change.full_clean()
            flight_to_change.save()
    except ValidationError:
        flight_to_change.crew = old_crew
        return HttpResponseForbidden("Załoga jest już zajęta w tym czasie.")
    return HttpResponse()


@require_GET
@csrf_exempt
def getCrewMembers(request):
    try:
        crew = Crew.objects.get(captain=request.GET['captain'])
    except Crew.DoesNotExist:
        return JsonResponse({'employees': list(Employee.objects.values_list('name', flat=True))})
    return JsonResponse({'members': list(crew.members.values_list('name', flat=True)),
                         'employees': list(Employee.objects.values_list('name', flat=True))})


# This should never fail if called from website
@require_POST
@csrf_exempt
def changeMembership(request):
    user = authenticate(username=request.POST['username'], password=request.POST['password'])
    if user is None:
        return HttpResponseForbidden("Użytkownik niezalogowany.")
    crew = get_object_or_404(Crew, captain=request.POST['captain'])
    employee = get_object_or_404(Employee, name=request.POST['employee'])
    if crew.captain == employee:
        return HttpResponseForbidden("Nie należy podawać kapitana w składzie załogi")
    if crew.members.filter(name=employee).exists():
        crew.members.remove(employee)
        crew.save()
        return HttpResponse()
    else:
        crew.members.add(employee)
        crew.save()
        return HttpResponse()


@require_POST
@csrf_exempt
def createNewCrew(request):
    user = authenticate(username=request.POST['username'], password=request.POST['password'])
    if user is None:
        return HttpResponseForbidden("Użytkownik niezalogowany.")
    captain = get_object_or_404(Employee, name=request.POST['captain'])
    Crew.objects.create(captain=captain)
    return HttpResponse()


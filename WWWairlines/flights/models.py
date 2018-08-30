from django.db import models
from django.core.validators import MinValueValidator
from django.core.exceptions import ValidationError
from django.db.models import Q


class Plane(models.Model):
    name = models.CharField(max_length=100, primary_key=True)
    capacity = models.IntegerField(validators=[MinValueValidator(1)], default=1)

    def __str__(self):
        return "%s (pojemność: %d)" % (self.name, self.capacity)


class Airport(models.Model):
    name = models.CharField(max_length=100, primary_key=True)

    def __str__(self):
        return "%s" % self.name


class Employee(models.Model):
    name = models.CharField(max_length=100, primary_key=True)
    captainPermissions = models.BooleanField(default=False)

    def __str__(self):
        return "%s" % self.name


class Crew(models.Model):
    captain = models.OneToOneField(Employee, on_delete=models.CASCADE, related_name="captain", primary_key=True)
    # There must be at least one, captain can't work alone
    members = models.ManyToManyField(Employee)

    def __str__(self):
        string = str(self.captain) + "(d)"
        for e in self.members.all():
            string += ", " + str(e)
        return string

    def clean(self):
        if not self.captain.captainPermissions:
            raise ValidationError("%s nie ma uprawnień kapitańskich" % self.captain)
        # https://stackoverflow.com/questions/37857003/django-admin-validation-on-manytomany-field
        # This validation doesn't work in admin panel
        # For solution check admin.py and forms.py
        if self.captain in self.members.all():
            raise ValidationError("Proszę nie podawać kapitana drugi raz w załodze")


class Flight(models.Model):
    departureAirport = models.ForeignKey(Airport, related_name="origin", on_delete=models.CASCADE)
    departureDate = models.DateTimeField()
    arrivalAirport = models.ForeignKey(Airport, related_name="destination", on_delete=models.CASCADE)
    arrivalDate = models.DateTimeField()
    plane = models.ForeignKey(Plane, on_delete=models.CASCADE)
    crew = models.ForeignKey(Crew, on_delete=models.CASCADE)

    def clean(self):
        if self.departureAirport == self.arrivalAirport:
            raise ValidationError("Nie można rozpocząć i zakończyć lotu w tym samym miejscu")
        if self.departureDate > self.arrivalDate:
            raise ValidationError("Nie można rozpocząć lotu po jego zakończeniu")
        # Check whether any flights overlap
        plane_collisions = Flight.objects.filter(plane=self.plane).exclude(id=self.id).filter(
            (Q(departureDate__gt=self.departureDate) & Q(departureDate__lt=self.arrivalDate)) |
            (Q(arrivalDate__gt=self.departureDate) & Q(arrivalDate__lt=self.arrivalDate))
        )
        if plane_collisions.exists():
            raise ValidationError("Samolot nie rozdwoi się")
        # Check whether crew has any other flight at the same time
        crew_collisions = Flight.objects.filter(crew=self.crew).exclude(id=self.id).filter(
            (Q(departureDate__gt=self.departureDate) & Q(departureDate__lt=self.arrivalDate)) |
            (Q(arrivalDate__gt=self.departureDate) & Q(arrivalDate__lt=self.arrivalDate))
        )
        if crew_collisions.exists():
            raise ValidationError("Załoga nie rozdwoi się")
        # This doesn't eliminate scenario: employee X is a member of crews A and B, which fly at the same time

    def __str__(self):
        return "Lot %d: %s (%s) -> %s (%s), %s" % (self.id, self.departureAirport, self.departureDate,
                                                   self.arrivalAirport, self.arrivalDate, self.plane)


class Person(models.Model):
    # https://www.w3.org/International/questions/qa-personal-names
    name = models.CharField(max_length=100, primary_key=True)

    def __str__(self):
        return "%s" % self.name


class Booking(models.Model):
    flight = models.ForeignKey(Flight, on_delete=models.CASCADE)
    person = models.ForeignKey(Person, on_delete=models.CASCADE)
    tickets = models.IntegerField(validators=[MinValueValidator(1)])

    def __str__(self):
        return "Rezerwacja: %s\n%s (%d biletów)" % (self.person, self.flight, self.tickets)

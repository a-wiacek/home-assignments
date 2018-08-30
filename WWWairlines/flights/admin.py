from django.contrib import admin
from flights.forms import CrewForm
from flights.models import *


class CrewAdmin(admin.ModelAdmin):
    form = CrewForm


# Register your models here.
admin.site.register(Plane)
admin.site.register(Airport)
admin.site.register(Flight)
admin.site.register(Employee)
admin.site.register(Crew, CrewAdmin)
admin.site.register(Person)
admin.site.register(Booking)

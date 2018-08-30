from django.core.exceptions import ValidationError
from django.forms import ModelForm
from django import forms
from .models import Booking, Crew
import datetime


class BookingForm(ModelForm):
    class Meta:
        model = Booking
        fields = ['person', 'tickets']
        labels = {
            'person': "Pasażer",
            'tickets': "Liczba biletów",
        }


class DataFilterForm(forms.Form):
    depMinDate = forms.SplitDateTimeField(initial=datetime.date.today, required=False, label="Data odlotu od:")
    depMaxDate = forms.SplitDateTimeField(initial=datetime.date.today, required=False, label="Data odlotu do:")
    arrMinDate = forms.SplitDateTimeField(initial=datetime.date.today, required=False, label="Data przylotu od:")
    arrMaxDate = forms.SplitDateTimeField(initial=datetime.date.today, required=False, label="Data przylotu do:")


class CrewForm(ModelForm):
    class Meta:
        model = Crew
        fields = ['captain', 'members']

    def clean(self):
        captain = self.cleaned_data.get('captain')
        members = self.cleaned_data.get('members')
        if captain and members:
            for member in members:
                if member == captain:
                    raise ValidationError("Proszę nie podawać kapitana drugi raz w załodze")
        return self.cleaned_data

"""WWWairlines URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/2.0/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
from django.contrib import admin
from django.urls import path, re_path
import flights.views

urlpatterns = [
    path('', flights.views.main),
    re_path(r'flight/([0-9]+)/', flights.views.flight, name="flight"),
    path('admin/', admin.site.urls),
    path('ajax/checkLoginStatus/', flights.views.checkLoginStatus, name="checkLoginStatus"),
    path('ajax/getFlightsOnDate/', flights.views.getFlightsOnDate, name="getFlightsOnDate"),
    path('ajax/changeCrewAssignment/', flights.views.changeCrewAssignment, name="changeCrewAssignment"),
    path('ajax/getCrewMembers/', flights.views.getCrewMembers, name="getCrewMembers"),
    path('ajax/changeMembership/', flights.views.changeMembership, name="changeMembership"),
    path('ajax/createNewCrew/', flights.views.createNewCrew, name="createNewCrew"),
]

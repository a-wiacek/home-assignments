let state = {
    is_logged_in: false
};

function revealOnLogin(username, password) {
    $('#username').text(username);
    $('.logged-true').show();
    localStorage.setItem("username", username);
    $('.logged-false').hide();
    localStorage.setItem("password", password);
    state.is_logged_in = true;
    triggerDisplayChange();
}

function hideOnLogout() {
    $('.logged-true').hide();
    localStorage.removeItem("username");
    $('.logged-false').show();
    localStorage.removeItem("password");
    state.is_logged_in = false;
    triggerDisplayChange();
}

function authenticate(username, password, writeAlert) {
    $.post('/ajax/checkLoginStatus/',
        {
            username: username,
            password: password
        }, function() {
            revealOnLogin(username, password);
        }).fail(function() {
            hideOnLogout();
            if (writeAlert)
                alert("Zły login lub hasło.");
        });
}

let dateStringOptions = {
    month: 'long',
    day: 'numeric',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
};

function getCrewCaptain(captainName, captainsArray, flightId) {
    if (!state.is_logged_in)
        return captainName;
    let str = '<select id="flight' + flightId + '">';
    for (let i = 0; i < captainsArray.length; ++i) {
        str += '<option value="' + captainsArray[i] + '"';
        if (captainsArray[i] === captainName)
            str += ' selected';
        str += '>' + captainsArray[i] + '</option>';
    }
    str += '</select>';
    return str;
}

function convertFlightToHTMLTableRow(flightObject, captainsArray) {
    return '<tr>' +
        '<td>' + flightObject.departureAirport_id + '</td>' +
        '<td>' + flightObject.arrivalAirport_id + '</td>' +
        '<td>' + flightObject.plane_id + '</td>' +
        '<td>' + new Date(flightObject.departureDate).toLocaleString('pl', dateStringOptions) + '</td>' +
        '<td>' + new Date(flightObject.arrivalDate).toLocaleString('pl', dateStringOptions) + '</td>' +
        '<td>' + getCrewCaptain(flightObject.crew_id, captainsArray, flightObject.id) + '</td>' +
        '</tr>';
}

function displayFlights(flightsArray, captainsArray)
{
    if (flightsArray.length === 0) {
        $('#no-flights-info').show();
        $('#flights-info').hide();
        return;
    }
    $('#no-flights-info').hide();
    let table_selector = $('#flights-info');
    table_selector.show();
    table_selector.find("tr:gt(0)").empty(); // remove all rows except for header
    for (let i = 0; i < flightsArray.length; ++i) {
        table_selector.append(convertFlightToHTMLTableRow(flightsArray[i], captainsArray));
        if (!state.is_logged_in)
            continue;
        let entry_selector = $('#flight' + flightsArray[i].id);
        entry_selector.data('cpt', flightsArray[i].crew_id);
        entry_selector.change(function() {
            $.post('/ajax/changeCrewAssignment/',
                {
                    username: localStorage.getItem("username"),
                    password: localStorage.getItem("password"),
                    flightId: flightsArray[i].id,
                    newCaptain: entry_selector.val()
                }, function() {
                    entry_selector.data('cpt', entry_selector.val());
                    alert("Pomyślnie zmieniono załogę lotu.");
                }).fail(function() {
                    entry_selector.val(entry_selector.data('cpt'));
                    alert("Nie udało się zmienić załogi. W tym czasie ma ona już jakiś lot do odbycia.");
                });
        });
    }
}

function triggerDisplayChange(ISOdate) {
    if (ISOdate) {
        $('#select-date').val(ISOdate).trigger('change');
    } else {
        $('#select-date').trigger('change');
    }
}

function displayCaptainsList(captainsArray) {
    let list_selector = $('#select-captain');
    list_selector.empty();
    for (let i = 0; i < captainsArray.length; ++i)
        list_selector.append('<option value="' + captainsArray[i] + '">' + captainsArray[i] + '</option>');
    list_selector.val(list_selector.find("option:first").val());
}

function displayCrewMembers(captain, membersArray, employeesArray) {
    if (!membersArray) { // Crew does not exist
        $.post('/ajax/createNewCrew/',
            {
                username: localStorage.getItem("username"),
                password: localStorage.getItem("password"),
                captain: captain
            });
        alert("Kapitan nie miał przypisanej do tej pory żadnej załogi. Utworzono nową, pustą załogę.");
        membersArray = [];
    }
    let list_selector = $('#select-members');
    list_selector.multiSelect('destroy');
    list_selector.empty();
    for (let i = 0; i < employeesArray.length; ++i) {
        let HTML = '<option value="' + employeesArray[i] + '"';
        if (membersArray.includes(employeesArray[i]))
            HTML += ' selected';
        else if (employeesArray[i] === captain) {
            HTML += ' selected disabled="disabled"';
        }
        HTML += '>' + employeesArray[i] + '</option>';
        if (employeesArray[i] === captain) {
            list_selector.prepend(HTML);
        } else {
            list_selector.append(HTML);
        }
    }
    list_selector.multiSelect(multiSelectOptions);
}

function addOrRemoveMembership(employeeAsArray) {
    $.post('/ajax/changeMembership/',
        {
            username: localStorage.getItem("username"),
            password: localStorage.getItem("password"),
            captain: $('#select-captain').find('option:selected').val(),
            employee: employeeAsArray[0]
        });
}

let multiSelectOptions = {
    selectableHeader: "Pracownicy",
    selectionHeader: "Członkowie załogi",
    afterSelect: function(employeeAsArray) {
        addOrRemoveMembership(employeeAsArray);
    },
    afterDeselect: function(employeeAsArray) {
        addOrRemoveMembership(employeeAsArray);
    }
};

$().ready(function() {
    $('#select-date').val(new Date().toISOString().substr(0, 10)); // today
    authenticate(
        localStorage.getItem("username"),
        localStorage.getItem("password"),
        false
    );

    $('#login-button').click(function() {
        authenticate(
            $('#input-username').val(),
            $('#input-password').val(),
            true
        );
    });

    $('#logout-button').click(function() {
        hideOnLogout();
    });

    $('#select-date').change(function() {
        let date = $('#select-date').val();
        if (date === '')
            return;
        $.get('/ajax/getFlightsOnDate/',
            {
                date: date
            }, function(dict) {
                displayFlights(dict['flights'], dict['captains']);
        });
    });

    $.get('/ajax/getFlightsOnDate/',
        {
            date: new Date().toISOString().substr(0, 10) // irrelevant value
        }, function(dict) {
            displayCaptainsList(dict['captains']);
    });

    // http://loudev.com
    $('#select-members').multiSelect(multiSelectOptions);
    $('#select-captain').change(function() {
        let captain = $('#select-captain option:selected').val();
        $.get('/ajax/getCrewMembers/',
            {
                captain: captain
            }, function(dict) {
                displayCrewMembers(captain, dict['members'], dict['employees']);
        });
    });
});
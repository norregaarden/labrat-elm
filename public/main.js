// https://www.elm-spa.dev/examples/03-storage
const app = Elm.Main.init({
  flags: JSON.parse(localStorage.getItem('storage'))
})

app.ports.save.subscribe(storage => {
  localStorage.setItem('storage', JSON.stringify(storage))
  app.ports.load.send(storage)
})

// https://www.keycloak.org/docs/latest/securing_apps/#_javascript_adapter
function initKeycloak() {
    var keycloak = new Keycloak();
    keycloak.init({
        onLoad: 'login-required'
        //onLoad: 'check-sso',
        //silentCheckSsoRedirectUri: window.location.origin + '/silent-check-sso.html'
    }).then(function(authenticated, subject) {
        alert(authenticated ? 'authenticated' + subject : 'not authenticated');
        keycloak.loadUserProfile()
            .then(function(profile) {
                alert(JSON.stringify(profile, null, "  "))
            }).catch(function() {
                alert('Failed to load user profile');
            });
    }).catch(function() {
        alert('failed to initialize');
    });
}
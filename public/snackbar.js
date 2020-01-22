
function snackbar(message) {
    var s = document.getElementById('snackbar');
    var data = {
        message: message,
        timeout: 2000
    };
    s.MaterialSnackbar.showSnackbar(data);
}

export { snackbar };
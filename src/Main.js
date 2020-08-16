

exports._fileFromInput = function(ev) {
  return function(onError, onSuccess) {
    const file = ev.target.files[0]
    var reader = new FileReader();
    reader.readAsText(file, "UTF-8");
    reader.onload = function (evt) {
      onSuccess(evt.target.result)
    }
    reader.onerror = function (evt) {
      onError(evt.target.error)
    }
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      // no way to cancel filereader
    }
  }
}
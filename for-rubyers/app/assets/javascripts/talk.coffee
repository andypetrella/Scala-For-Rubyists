class Talk
  constructor: (@el, @url) ->
    @title = @el.find("#title")
    @result = @el.find("#result")

  activate: (launcher) =>
    launcher.on "click", () =>
      $.get(@url).success((data) =>
        @title.html("Stress time : " + data.stress + ". Computation time : " + data.time)
        $(data.response).each (idx, item) =>
          li = $("<li>")
          li.append(item.time + " :: " + item.result)
          @result.append(li)
      )

window.Talk = Talk
(function() {
  var Colorizer;

  window.AWM.Classes.Colorizer = window.AWM.Classes.Colorizer || (Colorizer = (function() {
    function Colorizer(el, sun, options) {
      this.sun = sun;
      this.el = $(el);
      this.options = $.extend(true, {
        hue: {
          fn: function() {
            return Math.random();
          },
          min: 0,
          max: 1
        },
        saturation: {
          fn: function() {
            return Math.random();
          },
          min: 0,
          max: 1
        },
        lightness: {
          fn: function() {
            return Math.random();
          },
          min: 0,
          max: 1
        }
      }, options);
      console.log(this);
    }

    Colorizer.prototype.hue = function() {
      return this.options.hue.fn().map(this.options.hue.min, this.options.hue.max, 0, 360);
    };

    Colorizer.prototype.saturation = function() {
      return this.options.saturation.fn().map(this.options.saturation.min, this.options.saturation.max, 0, 100);
    };

    Colorizer.prototype.lightness = function() {
      return this.options.lightness.fn().map(this.options.lightness.min, this.options.lightness.max, 0, 100);
    };

    Colorizer.prototype.classify = function() {
      this.el.removeClass('dark');
      if (this.is_dark()) {
        return this.el.addClass('dark');
      }
    };

    Colorizer.prototype.hsl = function() {
      return {
        h: this.hue(),
        s: this.saturation(),
        l: this.lightness()
      };
    };

    Colorizer.prototype.is_dark = function() {
      return this.lightness < (this.options.lightness.min + this.options.lightness.max) / 2;
    };

    return Colorizer;

  })());

}).call(this);

(function() {
  var Ink;

  window.AWM.Classes.Ink = window.AWM.Classes.Ink || (Ink = (function() {
    function Ink(options) {
      this.options = $.extend({
        color: function() {
          return 'red';
        },
        splatter_threshold: 10,
        max_brush_width: 10,
        max_splats: 10,
        blotchiness: 10,
        canvas_size: window.innerWidth * 2,
        canvas_frame: $('.canvas-frame')
      }, options);
      this.distance_drawn = 0;
      this.canvas = $('<canvas />').attr({
        width: this.options.canvas_size * this.scale(),
        height: this.options.canvas_size * this.scale()
      });
      this.canvas.appendTo(this.options.canvas_frame);
      this.canvas.width(this.options.canvas_size);
      this.context = this.canvas[0].getContext('2d');
      this.context.lineJoin = "round";
      this.context.lineCap = "butt";
      this.listen();
      console.log(this);
    }

    Ink.prototype.listen = function() {
      return $(document).on('mousemove', (function(_this) {
        return function(e) {
          _this.track(e);
          return _this.draw();
        };
      })(this));
    };

    Ink.prototype.scale = function() {
      if (window.hasOwnProperty('devicePixelRatio')) {
        return window.devicePixelRatio;
      } else {
        return 1;
      }
    };

    Ink.prototype.delta = function(start, end) {
      if (start == null) {
        start = this.previous();
      }
      if (end == null) {
        end = this.current;
      }
      return Math.sqrt(Math.pow(start.y - end.y, 2) + Math.pow(start.x - end.x, 2));
    };

    Ink.prototype.time_elapsed = function() {
      return this.current.time - this.previous().time;
    };

    Ink.prototype.velocity = function() {
      return this.delta(this.current, this.previous()) / this.time_elapsed();
    };

    Ink.prototype.trajectory = function() {
      return {
        x: (this.current.x + (this.current.x - this.previous().x) * 2) * this.scale(),
        y: (this.current.y + (this.current.y - this.previous().y) * 2) * this.scale()
      };
    };

    Ink.prototype.stroke_width = function() {
      return (this.options.max_brush_width / Math.sqrt(this.velocity()).map(0, this.options.blotchiness, 1, this.options.blotchiness)) * this.scale();
    };

    Ink.prototype.previous = function() {
      return this.last_event || {
        x: window.innerWidth / 2,
        y: window.innerHeight / 2,
        time: new Date()
      };
    };

    Ink.prototype.draw = function(e) {
      this.distance_drawn += this.delta();
      this.context.beginPath();
      this.line(this.previous(), this.current, this.stroke_width());
      if (this.velocity() > this.options.splatter_threshold) {
        return this.splatter();
      }
    };

    Ink.prototype.track = function(e) {
      var now;
      now = {
        x: e.pageX,
        y: e.pageY,
        time: e.timeStamp
      };
      if (this.last_event == null) {
        this.last_event = this.current = now;
      } else {
        this.last_event = this.current;
        this.current = now;
      }
      return this.mileage += this.delta(this.current, this.previous());
    };

    Ink.prototype.colorize = function() {
      this.context.strokeStyle = this.options.color();
      return this.context.fillStyle = this.options.color();
    };

    Ink.prototype.line = function(from, to, width) {
      this.colorize();
      this.context.moveTo(from.x * this.scale(), from.y * this.scale());
      this.context.lineTo(to.x * this.scale(), to.y * this.scale());
      this.context.closePath();
      this.context.lineWidth = width;
      return this.context.stroke();
    };

    Ink.prototype.splatter = function() {
      var location, size;
      if (Math.random() > 0.5) {
        location = this.trajectory();
        size = Math.pow(this.velocity(), Math.random());
        return this.spot(location, size);
      }
    };

    Ink.prototype.spot = function(location, radius) {
      this.colorize();
      this.context.beginPath();
      this.context.arc(location.x, location.y, radius * this.scale(), 0, 2 * Math.PI);
      return this.context.fill();
    };

    Ink.prototype.clear = function() {
      return this.context.clearRect(0, 0, this.canvas.width, canvas.height);
    };

    Ink.prototype.blot = function(e) {
      var drop, i, ref, results;
      results = [];
      for (drop = i = 0, ref = Math.random() * this.options.max_splats; 0 <= ref ? i <= ref : i >= ref; drop = 0 <= ref ? ++i : --i) {
        results.push(this.spot({
          x: e.pageX.random_within(50),
          y: e.pageY.random_within(50)
        }, Math.sqrt(Math.random() * 1000)));
      }
      return results;
    };

    Ink.prototype.change_color = function(new_color) {
      this.context.strokeStyle = new_color;
      return this.context.fillStyle = new_color;
    };

    return Ink;

  })());

}).call(this);

(function() {
  var Sol;

  window.AWM.Classes.Sol = window.AWM.Classes.Sol || (Sol = (function() {
    function Sol(sunrise, sunset) {
      this.sunrise = sunrise;
      this.sunset = sunset;
      console.log(this);
    }

    Sol.prototype.now = function() {
      return new Date().getTime();
    };

    Sol.prototype.is_up = function() {
      var ref;
      return (this.sunset > (ref = this.now()) && ref > this.sunrise);
    };

    Sol.prototype.is_down = function() {
      return this.sunrise > this.now();
    };

    Sol.prototype.to_sunset = function() {
      return this.sunset - this.now();
    };

    Sol.prototype.to_sunrise = function() {
      return this.sunrise - this.now();
    };

    Sol.prototype.to_midnight = function() {
      return this.now();
    };

    Sol.prototype.day_length = function() {
      return this.sunset - this.sunrise;
    };

    Sol.prototype.day_mid = function() {
      return (this.sunset + this.sunrise) / 2;
    };

    Sol.prototype.day_progress = function() {
      return this.now() - this.day_mid();
    };

    Sol.prototype.day_progress_as_decimal = function() {
      return this.day_progress().map(-this.constants.day_half, this.constants.day_half, 0, 1);
    };

    Sol.prototype.approximate_brightness = function() {
      return Math.cos((2 * Math.PI * this.day_progress_as_decimal()) + Math.PI) / 2 + 0.5;
    };

    Sol.prototype.constants = {
      day: 1000 * 60 * 60 * 24,
      day_half: 1000 * 60 * 60 * 12
    };

    return Sol;

  })());

}).call(this);

(function() {
  window.AWM.Storage.Time = Number.prototype.map = function(in_min, in_max, out_min, out_max) {
    return (this - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
  };

  Number.prototype.constrain = function(min, max) {
    return Math.max(Math.min(this, min), max);
  };

  Number.prototype.random_within = function(distance) {
    return Math.random().map(0, 1, this - distance, this + distance);
  };

  window.AWM.Functions.document_height = function() {
    var body, body_element;
    body = document.body;
    body_element = document.documentElement;
    return Math.max(body.scrollHeight, body.offsetHeight, body_element.clientHeight, body_element.scrollHeight, body_element.offsetHeight);
  };

  $.extend(Date.prototype, {
    SECOND_IN_MILLISECONDS: 1000,
    MINUTE_IN_SECONDS: 60,
    MINUTE_IN_MILLISECONDS: 60 * 1000,
    HOUR_IN_SECONDS: 60 * 60,
    HOUR_IN_MILLISECONDS: 60 * 60 * 1000,
    DAY_IN_SECONDS: 24 * 60 * 60,
    DAY_IN_MILLISECONDS: 24 * 60 * 60 * 1000,
    WEEK_IN_SECONDS: 7 * 24 * 60 * 60,
    WEEK_IN_MILISECONDS: 7 * 24 * 60 * 60 * 1000
  });

  Date.prototype.weekStart = function() {
    return this.getTime() - (this.getDay() * this.DAY_IN_MILLISECONDS) - (this.getHours() * this.HOUR_IN_MILLISECONDS) - (this.getMinutes() * this.MINUTE_IN_MILLISECONDS) - (this.getSeconds() * this.SECOND_IN_MILLISECONDS) - (this.getMilliseconds());
  };

  Date.prototype.weekEnd = function() {};

}).call(this);

(function() {
  var indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

  $(function() {

    /*
      Hi, I'm glad you're here.
     */

    /*
      Let's keep track of where we are in the day.
     */
    window.AWM.UI.Sun = new window.AWM.Classes.Sol(window.AWM.Storage.sunrise, window.AWM.Storage.sunset);

    /*
      This will help us generate meaningful HSL values from incoming data.
      Psst! See where this is going?
     */
    window.AWM.UI.Color = new window.AWM.Classes.Colorizer('body', window.AWM.UI.Sun, {
      hue: {
        fn: function() {
          return new Date().getTime() % 36000;
        },
        min: 0,
        max: 36000
      },
      saturation: {
        fn: function() {
          return 1;
        },
        min: 0,
        max: 1
      },
      lightness: {
        fn: function() {
          return window.AWM.UI.Sun.approximate_brightness();
        },
        min: 0,
        max: 1.5
      }
    });

    /*
      From time to time, we might update the color palette, like as the sun goes down.
     */
    window.AWM.Functions.update_color = function() {
      var color;
      return color = window.AWM.Storage.color_current = window.AWM.UI.Color.hsl();
    };
    window.AWM.Storage.color_on_load = window.AWM.UI.Color.hsl();
    window.AWM.Functions.update_color();
    window.setInterval(window.AWM.Functions.update_color, 500);

    /*
      I thought this might be fun to play with.
     */
    if (indexOf.call(window, 'ontouchstart') < 0) {
      return window.AWM.UI.Pen = new window.AWM.Classes.Ink({
        color: function() {
          var color;
          color = window.AWM.UI.Color.hsl();
          return "hsl(" + ((color.h - 40) % 360) + ", " + color.s + "%, " + (color.l.map(0, 100, 50, 100)) + "%)";
        },
        canvas_unsupported: window.AWM.Storage.canvas_unsupported,
        splatter_threshold: 1.5,
        max_brush_width: 10,
        blotchiness: 15,
        canvas_size: Math.max(window.innerWidth, window.AWM.Functions.document_height()),
        canvas_frame: $('.canvas-frame')
      });
    }
  });

}).call(this);

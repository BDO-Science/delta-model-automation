// Thousands separators for editable flow and volume inputs.
// Applies only to numeric-like text fields, not names, dates, or selectors.
// The R model parser already strips commas before numeric conversion.

var ccwdNumberInputIds =
  /^(?:(?:current|forecast)_(?:ptm_(?:exp|ver|sac|east|qgcd|qprec|dcc_flow|xgeo)|eco_(?:sac|yol|moke))|compare_s[12]_(?:exp|ver|sac|east|qgcd|qprec|dcc_flow|yol|moke))$/;

      // Thousands separators for editable flow and volume inputs.
      // Applies only to numeric-like text fields, not names, dates, or selectors.
      // The R model parser already strips commas before numeric conversion.
      var ccwdNumberInputIds = /^(?:(?:current|forecast)_(?:ptm_(?:exp|ver|sac|east|qgcd|qprec|dcc_flow|xgeo)|eco_(?:sac|yol|moke))|compare_s[12]_(?:exp|ver|sac|east|qgcd|qprec|dcc_flow|yol|moke))$/;
      function ccwdCommaFormat(raw) {
        var v=String(raw).replace(/,/g,'').replace(/\\s/g,'');
        if (!/^-?\\d*(?:\\.\\d*)?$/.test(v)) return raw;
        var parts=v.split('.');
        parts[0]=parts[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g,',');
        return parts.join('.');
      }
      $(document).on('input change', 'input[type=text]', function() {
        if (!ccwdNumberInputIds.test(this.id) || this.readOnly) return;
        var before=this.value, after=ccwdCommaFormat(before);
        if (before===after) return;
        var start=this.selectionStart, end=this.selectionEnd;
        this.value=after;
        if (start!=null && end!=null) {
          var newStart=ccwdCommaFormat(before.substring(0,start)).length;
          var newEnd=ccwdCommaFormat(before.substring(0,end)).length;
          try {this.setSelectionRange(newStart,newEnd);} catch(e) {}
        }
      });
      // Server-driven defaults are applied after the initial input event.
      // Format those too, without triggering new Shiny input updates.
      $(document).on('shiny:inputchanged', function(e) {
        if (!e.name || !ccwdNumberInputIds.test(e.name)) return;
        var el=document.getElementById(e.name);
        if (!el || el.type!=='text' || el===document.activeElement) return;
        el.value=ccwdCommaFormat(el.value);
      });
      $(function() {
        $('input[type=text]').filter(function(){return ccwdNumberInputIds.test(this.id);}).each(function(){
          this.value=ccwdCommaFormat(this.value);
        });
      });

      function resizeLeafletMaps() {
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 250);
    
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 750);
      }
    
      $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function() {
        resizeLeafletMaps();
      });
    
      $(document).on('shown.bs.tab', 'a[data-toggle=\"pill\"]', function() {
        resizeLeafletMaps();
      });
    
      $(document).on('click', '.sidebar-menu a', function() {
        resizeLeafletMaps();
      });

      // Exploration/Weekly/Comparison information popovers: render a read-only
      // visual copy directly under <body>. This avoids clipping by the
      // horizontally scrollable inputs row, tab panels, or dashboard margins.
      // The original Shiny UI output stays in place and remains reactive.
      var ccwdInfoPopup = null;
      var ccwdInfoHost = null;
      function ccwdHideInfoPopup() {
        if (ccwdInfoPopup) ccwdInfoPopup.remove();
        ccwdInfoPopup = null;
        ccwdInfoHost = null;
      }
      function ccwdPositionInfoPopup() {
        if (!ccwdInfoPopup || !ccwdInfoHost || !ccwdInfoHost.length) return;
        var anchor = ccwdInfoHost.find('.flow-info-trigger').first()[0];
        if (!anchor) return;
        var rect = anchor.getBoundingClientRect();
        var width = Math.min(420, window.innerWidth - 24);
        ccwdInfoPopup.css({width: width + 'px', maxWidth: width + 'px'});
        var height = ccwdInfoPopup.outerHeight();
        var left = Math.min(Math.max(12, rect.left - 4), window.innerWidth - width - 12);
        var below = rect.bottom + 8;
        var top = (below + height <= window.innerHeight - 12 || below < 70)
          ? below : Math.max(12, rect.top - height - 8);
        top = Math.max(12, Math.min(top, window.innerHeight - 30));
        ccwdInfoPopup.css({left: left + 'px', top: top + 'px'});
      }
      function ccwdShowInfoPopup(host) {
        ccwdHideInfoPopup();
        ccwdInfoHost = $(host);
        var source = ccwdInfoHost.children('.flow-info-popover').first();
        if (!source.length) return;
        ccwdInfoPopup = source.clone(false, false);
        // Cloned informational content is inert. Avoid duplicate Shiny output IDs.
        ccwdInfoPopup.find('[id]').addBack('[id]').removeAttr('id');
        ccwdInfoPopup.removeClass('flow-info-popover')
          .addClass('ccwd-viewport-popover').appendTo('body');
        ccwdPositionInfoPopup();
      }
      $(document).on('mouseenter focusin', '.flow-info-hover', function() {
        ccwdShowInfoPopup(this);
      });
      $(document).on('mouseleave', '.flow-info-hover', function() {
        if (ccwdInfoHost && ccwdInfoHost[0] === this) ccwdHideInfoPopup();
      });
      $(document).on('focusout', '.flow-info-hover', function() {
        var host = this;
        setTimeout(function() {
          if (!host.contains(document.activeElement) &&
              ccwdInfoHost && ccwdInfoHost[0] === host) ccwdHideInfoPopup();
        }, 0);
      });
      $(window).on('resize scroll', ccwdPositionInfoPopup);
      $('.content-wrapper, .exploration-ptm-grid').on('scroll', ccwdPositionInfoPopup);

      var modelRunWaiting = false;
      var modelRunBecameBusy = false;

      $(function() {
        $('body').tooltip({
          selector: '.flow-info-icon',
          container: 'body'
        });
      });

      // Paint the comparison controls before starting blocking external HTTP reads.
      // These requests are independent of a user's Run Scenario click.
      $(document).one('shiny:connected', function() {
        if (!window.Shiny || !Shiny.setInputValue) return;
        window.setTimeout(function() {
          Shiny.setInputValue('ccwd_load_observed', true, {priority:'event'});
        }, 350);
        window.setTimeout(function() {
          Shiny.setInputValue('ccwd_load_noaa', true, {priority:'event'});
        }, 750);
        window.setTimeout(function() {
          Shiny.setInputValue('ccwd_load_dcc', true, {priority:'event'});
        }, 1150);
      });

      // Persistent per-scenario status: queued, processing, slow, success or error.
      // These timers run in the browser even while R is busy with synchronous models.
      var compareTimers = {};
      var compareStarts = {};
      function compareStatus(id, state, message) {
        var $panel = $('#run_compare_' + id).closest('.compare-scenario-panel');
        var $status = $('#compare_' + id + '_status');
        $status.attr('data-state', state);
        var icons = {queued:'fa-clock-o',running:'fa-spinner fa-spin',slow:'fa-exclamation-circle',
                     success:'fa-check-circle',error:'fa-exclamation-triangle'};
        $status.empty().append($('<i/>', {'class':'fa ' + (icons[state] || 'fa-info-circle'), 'aria-hidden':'true'}))
          .append($('<span/>').text(message));
        $panel.toggleClass('is-comparing', state === 'queued' || state === 'running' || state === 'slow');
      }
      function finishCompareTimer(id) {
        if (compareTimers[id]) window.clearInterval(compareTimers[id]);
        delete compareTimers[id];
      }
      $(document).on('click', '.compare-run-action', function() {
        var id = this.id === 'run_compare_s1' ? 's1' : 's2';
        if (compareTimers[id]) return;
        compareStarts[id] = Date.now();
        compareStatus(id, 'queued', 'Request sent. Waiting for R to start processing... 0 s');
        compareTimers[id] = window.setInterval(function() {
          var sec = Math.floor((Date.now()-compareStarts[id])/1000);
          if (sec >= 90) {
            compareStatus(id, 'slow', 'Still waiting after ' + sec + ' s. The app may be busy or a data/model operation may be stalled. Check the RStudio Console for errors; this message does not cancel the run.');
          } else if (sec >= 30) {
            compareStatus(id, 'slow', 'Processing for ' + sec + ' s. Longer than expected; check the RStudio Console if it continues.');
          } else {
            compareStatus(id, 'running', 'Running scenario models... ' + sec + ' s elapsed.');
          }
        }, 1000);
      });
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('ccwdComparisonStatus', function(data) {
          if (!data || !data.id) return;
          var id = data.id;
          if (data.state === 'running') return; // Keep client elapsed clock running.
          finishCompareTimer(id);
          compareStatus(id, data.state || 'error', data.message || 'Unknown model status.');
        });
      }

      // Map requests are synchronous on the R side. Keep the progress timer on
      // the browser side, and finish it only when the matching Shiny output
      // returns a value/error. A long interval is a warning, not a false success.
      var ccwdMapTasks = {
        generate_library_map: {key:'library', output:'comparison_spatial_map'},
        generate_omri_map: {key:'omri', output:'omri_comparison_spatial_map'},
        generate_compare_two_map: {key:'compare_two', output:'compare_two_spatial_map'}
      };
      var ccwdActiveMaps = {};
      function ccwdSetMapStatus(key, state, description) {
        var task = ccwdActiveMaps[key];
        var $overlay = $('#' + key + '_map_progress');
        var $msg = $('#' + key + '_map_message');
        $overlay.toggleClass('is-active', state === 'running');
        $overlay.find('.comparison-map-progress-detail').text(description);
        $msg.attr('data-state', state).text(description);
        if (task) {
          var $button = $('#' + task.button);
          $button.prop('disabled', state === 'running');
        }
      }
      function ccwdStopMap(key, state, message) {
        var task=ccwdActiveMaps[key];
        if (!task) return;
        window.clearInterval(task.interval);
        ccwdSetMapStatus(key, state, message);
        delete ccwdActiveMaps[key];
      }
      $(document).on('click', '.comparison-map-action', function() {
        var button=this.id, config=ccwdMapTasks[button];
        if (!config || ccwdActiveMaps[config.key]) return;
        var key=config.key, started=Date.now();
        ccwdActiveMaps[key]={button:button, output:config.output, started:started};
        ccwdSetMapStatus(key, 'running', 'Map requested. Preparing spatial data...');
        ccwdActiveMaps[key].interval=window.setInterval(function() {
          var elapsed=Math.floor((Date.now()-started)/1000);
          var description='Generating spatial map... '+elapsed+' seconds elapsed.';
          if (elapsed>=300) description+=' This is taking over five minutes; check the RStudio Console for an error or heavy spatial computation.';
          else if (elapsed>=90) description+=' Spatial interpolation is taking longer than expected; the calculation may still be running.';
          ccwdSetMapStatus(key, 'running', description);
        },1000);
      });
      $(document).on('shiny:value', function(event) {
        var name=event.name || (event.target && event.target.id);
        Object.keys(ccwdActiveMaps).forEach(function(key) {
          if (ccwdActiveMaps[key].output===name)
            ccwdStopMap(key,'success','Map response received. The map is ready to view.');
        });
      });
      $(document).on('shiny:error', function(event) {
        var name=event.name || (event.target && event.target.id);
        Object.keys(ccwdActiveMaps).forEach(function(key) {
          if (ccwdActiveMaps[key].output===name) {
            function readableMapError(error) {
              if (error == null) return 'Unknown map error; check the RStudio Console.';
              if (typeof error === 'string') return error;
              if (Array.isArray(error)) return error.map(readableMapError).join('; ');
              if (typeof error.message === 'string') return error.message;
              if (typeof error.error === 'string') return error.error;
              if (error.error) return readableMapError(error.error);
              try { return JSON.stringify(error); } catch (e) { return 'See RStudio Console for details.'; }
            }
            var info=readableMapError(event.error || event.message || null);
            ccwdStopMap(key,'error','Spatial map failed: '+info);
          }
        });
      });
      $(document).on('shiny:disconnected',function() {
        Object.keys(ccwdActiveMaps).forEach(function(key) {
          ccwdStopMap(key,'error','R session disconnected before the map was generated.');
        });
      });

      // OMRI progress is independent of chart rendering and library maps.
      var omriStarted = 0, omriTimer = null;
      function finishOMRITimer() { if (omriTimer) window.clearInterval(omriTimer); omriTimer = null; }
      function setOMRIStatus(state, msg) {
        var el = $('#omri_run_status_live');
        var icons = {running:'fa-spinner fa-spin', slow:'fa-exclamation-circle',
          success:'fa-check-circle', error:'fa-exclamation-triangle'};
        el.attr('data-state', state).empty()
          .append($('<i/>', {'class':'fa '+(icons[state]||'fa-info-circle')}))
          .append($('<span/>').text(msg));
      }
      $(document).on('click', '#build_omri_comparison', function() {
        finishOMRITimer(); omriStarted=Date.now();
        setOMRIStatus('running', 'OMRI run requested; waiting for R... 0 s');
        omriTimer=window.setInterval(function() {
          var sec=Math.floor((Date.now()-omriStarted)/1000);
          setOMRIStatus(sec>=30?'slow':'running', sec>=30?
            ('Running OMRI scenarios for '+sec+' s. The server may be retrieving archived CSVs or calculating predictions; see the RStudio Console for per-scenario progress.'):
            ('Running OMRI scenarios... '+sec+' s elapsed.'));
        }, 1000);
      });
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('ccwdOMRIStatus', function(data) {
          if (!data) return;
          if (data.state === 'running') return;
          finishOMRITimer(); setOMRIStatus(data.state||'error', data.message||'Run ended without details.');
        });
      }

      $(document).on('shiny:disconnected', function() {
        if (omriTimer) {
          finishOMRITimer();
          setOMRIStatus('error', 'Connection to R was lost before completion. Check the RStudio Console.');
        }
        ['s1','s2'].forEach(function(id) {
          if (compareTimers[id]) {
            finishCompareTimer(id);
            compareStatus(id, 'error', 'Disconnected from the R session before completion. Reconnect and check the RStudio Console.');
          }
        });
      });

      $(document).on('click', '.model-run-button', function() {
        modelRunWaiting = true;
        modelRunBecameBusy = false;
        $('body').addClass('model-run-requested');
      });

      $(document).on('shiny:busy', function() {
        if (modelRunWaiting) {
          modelRunBecameBusy = true;
          $('body').addClass('model-run-requested');
        }
      });

      $(document).on('shiny:idle', function() {
        if (modelRunWaiting && modelRunBecameBusy) {
          setTimeout(function() {
            $('body').removeClass('model-run-requested');
            modelRunWaiting = false;
            modelRunBecameBusy = false;
          }, 150);
        }
      });

	$(document).on('shiny:idle', function() {
  if (modelRunWaiting && modelRunBecameBusy) {
    setTimeout(function() {
      $('body').removeClass('model-run-requested');
      modelRunWaiting = false;
      modelRunBecameBusy = false;
    }, 150);
  }
});
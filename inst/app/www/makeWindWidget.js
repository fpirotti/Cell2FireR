var isDragging = false;


var makeWindWidget = function(){
      var canvas = document.getElementById('windCanvas');
      var ctx = canvas.getContext('2d');
      var angle = 0;
      var speed = $('#speed').val();
      var centerX = canvas.width / 2;
      var centerY = canvas.height / 2;
      var radius = 72;
    
      var angle = 90; // wind direction (FROM, meteorological)

    
      function drawCompass() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);
    
        // --- Outer circle ---
        ctx.beginPath();
        ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI);
        ctx.stroke();
    
        // --- Cardinal directions ---
        ctx.font = "11px Arial";
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
    
        ctx.fillText("N", centerX, centerY - radius + 10);
        ctx.fillText("E", centerX + radius - 10, centerY);
        ctx.fillText("S", centerX, centerY + radius - 10);
        ctx.fillText("W", centerX - radius + 10, centerY);
    
        // --- Tick marks every 30° ---
        for (let i = 0; i < 360; i += 30) {
          let rad = (i - 90) * Math.PI / 180;
          let x1 = centerX + (radius - 5) * Math.cos(rad);
          let y1 = centerY + (radius - 5) * Math.sin(rad);
          let x2 = centerX + radius * Math.cos(rad);
          let y2 = centerY + radius * Math.sin(rad);
    
          ctx.beginPath();
          ctx.moveTo(x1, y1);
          ctx.lineTo(x2, y2);
          ctx.stroke();
        }
    
        // --- Wind arrow (FROM direction) ---
        let rad = (angle - 90) * Math.PI / 180;
    
        ctx.save();
        ctx.translate(centerX, centerY);
        ctx.rotate(rad);
    
        // arrow head
        ctx.beginPath();
        ctx.moveTo( radius - 15, 0);
        ctx.lineTo(  radius - 5, -5);
        ctx.lineTo(  radius - 5, 5);
        ctx.closePath();
        ctx.fillStyle = "red";
        ctx.fill();
          
        var speedr = parseInt(speed)+10; 
        ctx.beginPath(); 
        ctx.moveTo(0,0);
        ctx.lineTo(-speedr*2,0); // arrow length proportional to speed
        ctx.moveTo(-speedr,0);
        ctx.lineTo(-speedr+5,5);
        ctx.moveTo(-speedr,0);
        ctx.lineTo(-speedr+5,-5); 
        
        ctx.moveTo(-speedr*1.5,0);
        ctx.lineTo(-speedr*1.5+5,5);
        ctx.moveTo(-speedr*1.5,0);
        ctx.lineTo(-speedr*1.5+5,-5); 
        ctx.strokeStyle = "blue";
        ctx.lineWidth = 3;
        ctx.stroke();
     
    
        ctx.restore();
     
      }
 
     
      drawCompass();

      // Update speed
      $('#speed').on('input change', function(e){
        speed = $(this).val();
        $('#wspeed').text(speed);
        drawCompass();  
      });
      
      // Update dir
      $('#wdir').on('change', function(e){
        angle =  parseInt($('#wdir').text());
        drawCompass();  
      });
       

      var updateAngle = function(e){
        var rect = canvas.getBoundingClientRect();
        var x = e.clientX - rect.left - centerX;
        var y = e.clientY - rect.top - centerY;
    
        // Convert to meteorological angle (FROM direction)
        var theta = Math.atan2(y, x) * 180 / Math.PI;
        angle = (theta + 90 + 360) % 360; 
        drawCompass(); 
      }

    
      
   $('#windCanvas')
      .on('mousedown', function(e) {
        isDragging = true;
        updateAngle(e);
      })
      .on('mousemove', function(e) {
        if (isDragging) {
          updateAngle(e);
        }
      })
      .on('mouseleave mouseup', function() {
        $('#wdir').text(Math.round(angle)); 
        isDragging = false;
      });
      
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     
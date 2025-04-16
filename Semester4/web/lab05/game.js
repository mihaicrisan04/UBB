$(document).ready(function() {
    // Game variables
    let score = 0;
    let lives = 10;
    let gameInterval;
    let isGameOver = false;
    const gameArea = $('#gameArea');
    const basket = $('#basket');
    const scoreDisplay = $('#score');
    const livesDisplay = $('#lives');
    
    // Game constants
    const SHAPE_SIZE = 30;
    const BASKET_WIDTH = 100;
    const BASKET_HEIGHT = 50;
    const GAME_SPEED = 20; // milliseconds
    const SHAPE_SPAWN_INTERVAL = 1000; // milliseconds
    const BASKET_MOVE_SPEED = 40; // pixels per key press
    
    // Initialize basket position
    basket.css({
        left: '50%',
        bottom: '20px'
    });
    
    // Function to move basket
    function moveBasket(newX) {
        if (isGameOver) return;
        
        // Keep basket within game area bounds
        const maxX = gameArea.width() - BASKET_WIDTH;
        const boundedX = Math.max(0, Math.min(newX, maxX));
        
        basket.css('left', boundedX + 'px');
    }
    
    // Mouse movement handler
    $(document).mousemove(function(e) {
        const gameAreaOffset = gameArea.offset();
        const mouseX = e.pageX - gameAreaOffset.left;
        moveBasket(mouseX);
    });
    
    // Keyboard controls
    $(document).keydown(function(e) {
        const currentPosition = parseInt(basket.css('left'));
        
        switch(e.key) {
            case 'ArrowLeft':
                moveBasket(currentPosition - BASKET_MOVE_SPEED);
                break;
            case 'ArrowRight':
                moveBasket(currentPosition + BASKET_MOVE_SPEED);
                break;
        }
    });
    
    // Create a new falling shape
    function createShape() {
        if (isGameOver) return;
        
        const shape = $('<div>').addClass('shape');
        const startX = Math.random() * (gameArea.width() - SHAPE_SIZE);

        const isAtmoicBomb = Math.random() < 0.1;

        if (isAtmoicBomb) {
            shape.addClass('atomic-bomb');
        }
        
        shape.css({
            left: startX + 'px',
            top: -SHAPE_SIZE + 'px'
        });
        
        gameArea.append(shape);
        
        // Random speed between 2 and 5
        const speed = 2 + Math.random() * 3;

        let shapeScore = 1;
        if (speed > 3) {
            shapeScore = 2;
        } else {
            shapeScore = 1;
        }

        
        // Animate the shape falling
        shape.animate({
            top: gameArea.height() + 'px'
        }, {
            duration: 10000 / speed,
            step: function(now) {
                // Check for collision with basket
                const shapePos = $(this).position();
                const basketPos = basket.position();
                
                if (shapePos.top + SHAPE_SIZE >= basketPos.top &&
                    shapePos.left + SHAPE_SIZE >= basketPos.left &&
                    shapePos.left <= basketPos.left + BASKET_WIDTH) {
                    if (isAtmoicBomb) {
                        lives -= 999;
                        gameOver();
                    } else {
                        // Shape caught
                        $(this).remove();
                        score += shapeScore;
                        scoreDisplay.text('Score: ' + score);
                    }
                }
            },
            complete: function() {
                // Shape reached bottom without being caught
                $(this).remove();
                lives--;
                livesDisplay.text('Lives: ' + lives);
                
                if (lives <= 0) {
                    gameOver();
                }
            }
        });
    }
    
    // Start spawning shapes
    function startGame() {
        gameInterval = setInterval(createShape, SHAPE_SPAWN_INTERVAL);
    }
    
    // Game over function
    function gameOver() {
        isGameOver = true;
        clearInterval(gameInterval);
        $('.shape').remove();
        
        // Display game over message
        const gameOverMessage = $('<div>').css({
            position: 'fixed',
            top: '50%',
            left: '50%',
            transform: 'translate(-50%, -50%)',
            fontSize: '48px',
            color: '#e74c3c',
            textAlign: 'center'
        }).text('Game Over!\nFinal Score: ' + score);
        
        gameArea.append(gameOverMessage);
    }
    
    // Start the game
    startGame();
}); 
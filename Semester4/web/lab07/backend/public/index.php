<?php
use Psr\Http\Message\ServerRequestInterface as Request;
use Psr\Http\Server\RequestHandlerInterface;
use Psr\Http\Message\ResponseInterface;
use Slim\Factory\AppFactory;

require __DIR__ . '/../vendor/autoload.php';

$app = AppFactory::create();

// 1) Handle preflight requests globally
$app->options('/{routes:.+}', function (Request $request, ResponseInterface $response) {
    return $response;
});

// 2) Add CORS headers to all responses
$app->add(function (Request $request, RequestHandlerInterface $handler) {
    /** @var ResponseInterface $response */
    $response = $handler->handle($request);
    return $response
        ->withHeader('Access-Control-Allow-Origin', '*')
        ->withHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization')
        ->withHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, PATCH, DELETE, OPTIONS');
});

// example route
$app->get('/api/hello', function (Request $req, ResponseInterface $res) {
    $res->getBody()->write(json_encode(['message' => 'Hello!']));
    return $res->withHeader('Content-Type','application/json');
});

$app->run();

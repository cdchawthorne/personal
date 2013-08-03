#include <Box2D/Box2D.h>
#include <iostream>

int main()
{
    /* 
     * Initialization: 
     * Initialize world
     * Initialize ground
     * Initialize dynamic body
     */
    b2Vec2 gravity(0.0f, -10.0f);
    bool doSleep = true;
    //b2World world(gravity, doSleep);
    b2World world(gravity);

    b2BodyDef groundBodyDef;
    groundBodyDef.position.Set(0.0f, -10.0f);

    b2Body * groundBody = world.CreateBody(&groundBodyDef);

    b2PolygonShape groundBox;
    groundBox.SetAsBox(50.0f, 10.0f);

    /*
     * Shortcut fixture-creation method, given shape and density
     * Apparently the ground has no mass. =/
     *
     * groundBox is copied
     */
    groundBody->CreateFixture(&groundBox, 0.0f);

    /* Apparently the default type is static, as used in groundBody */
    b2BodyDef bodyDef;
    bodyDef.type = b2_dynamicBody;
    bodyDef.position.Set(0.0f, 4.0f);
    b2Body * body = world.CreateBody(&bodyDef);

    b2PolygonShape dynamicBox;
    dynamicBox.SetAsBox(1.0f, 1.0f);

    b2FixtureDef fixtureDef;
    fixtureDef.shape = &dynamicBox;
    fixtureDef.density = 1.0f;
    fixtureDef.friction = 0.3f;

    body->CreateFixture(&fixtureDef);

    /* Simulation */
    float32 timeStep = 1.0f / 60.0f;
    int32 velocityIterations = 8;
    int32 positionIterations = 3;

    for (int32 i = 0; i < 60; ++i) {
        world.Step(timeStep, velocityIterations, positionIterations);
        b2Vec2 position = body->GetPosition();
        float32 angle = body->GetAngle();
        std::cout << "Position: " <<  position.x << ", " << position.y << std::endl;
        std::cout << "Angle: " << angle << std::endl;
    }

    return 0;
}

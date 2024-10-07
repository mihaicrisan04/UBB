package model;

public class Apple extends Item {
    private String breed;

    public Apple(){
        this(0,"");

    }

    public Apple(float weight, String breed){
        super(weight);
        this.breed=breed;
    }

    public String getBreed(){
        return breed;
    }

    @Override
    public String toString(){
        return "Apple:[breed: "+breed+", "+super.toString()+"]";
    }

}
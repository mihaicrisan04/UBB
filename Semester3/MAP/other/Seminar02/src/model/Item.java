package model;

public abstract class Item {
    protected float weight;

    public float getWeight(){
        return weight;
    }

    public void setWeight(float weight) {
        this.weight = weight;
    }

    public Item(float weight){
        this.weight=weight;
    }



    public String toString(){
        return "weight: "+weight;
    }//Object

}
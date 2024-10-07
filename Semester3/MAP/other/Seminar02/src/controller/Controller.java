package controller;

import model.Item;
import repository.CapacityExceededException;
import repository.IRepository;

public class Controller{
    private IRepository repo;

    public Controller(IRepository repo){
        this.repo=repo;
    }

    public void add (Item f){
        try {
            repo.add(f);
        } catch (CapacityExceededException e) {
            System.out.println(e);
        }
    }

    public Item[] all(){
        return repo.all();
    }

    public int size(){
        return repo.size();
    }

    public Item[] filter(float weight){
        Item[] items1=new Item[repo.size()];
        System.out.println(items1.length);
        int size1=0;
//        for (int i=0; i<repo.size();i++){
//            Item item= repo.all()[i];
//           if(item.getWeight()>weight)
//               items1[size1++]=item;
//        }
//        return items1;

        for (Item item:repo.all()){
           if(item.getWeight()>weight)
               items1[size1++]=item;
        }
        return items1;
    }
}
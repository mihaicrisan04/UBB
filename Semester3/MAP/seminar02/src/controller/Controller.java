package controller;

import model.Item;
import repository.CapacityExceededException;
import repository.IRepository;

public class Controller {
    private IRepository repo;

    public Controller(IRepository repo) {
        this.repo = repo;
    }

    public void add(Item f) {
        try {
            repo.add(f);
        } catch (CapacityExceededException e) {
           System.out.println(e);
        }
    }

    public Item[] all() {
        return repo.all();
    }

    public int size() {
        return repo.size();
    }

    public Item[] filter(float weight) {
        Item[] filteredItems = new Item[repo.size()];
        int k = 0;
        for (Item item: repo.all()) {
            if (item.getWeight() > weight) {
                filteredItems[k++] = item;
            }
        }
        return filteredItems;
    }
}

/*
1. Intr-o parcare exista masini, motociclete
        si biciclete. Sa se afiseze toate vehiculele
        de culoare rosie.

 */

package repository;

import model.Vehicle;

public interface Repo {
    public void add(Vehicle vehicle);

    public void remove(Vehicle vehicle);

    public Vehicle[] getAll();
}

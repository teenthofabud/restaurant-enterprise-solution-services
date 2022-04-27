package com.teenthofabud.restaurant.solution.reservation.category.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.reservation.category.data.CategoryDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CategoryRepository extends MongoRepository<CategoryDocument, String>, TOABBaseMongoRepository {

    public Boolean existsByName(String name);

    public List<CategoryDocument> findByNameContaining(String name);

}

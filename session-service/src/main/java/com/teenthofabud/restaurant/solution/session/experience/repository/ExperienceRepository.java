package com.teenthofabud.restaurant.solution.session.experience.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.session.experience.data.ExperienceDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ExperienceRepository extends MongoRepository<ExperienceDocument, String>, TOABBaseMongoRepository {

    public Boolean existsByName(String name);

    public List<ExperienceDocument> findByNameContaining(String name);

}

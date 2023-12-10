package com.teenthofabud.restaurant.solution.settings.internationalization.repository;

import com.teenthofabud.core.common.repository.TOABBaseMongoRepository;
import com.teenthofabud.restaurant.solution.settings.internationalization.data.LanguageDocument;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface LanguageRepository extends MongoRepository<LanguageDocument, String>, TOABBaseMongoRepository {

    Boolean existsByName(String name);

}

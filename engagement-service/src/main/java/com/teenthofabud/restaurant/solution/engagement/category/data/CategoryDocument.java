package com.teenthofabud.restaurant.solution.engagement.category.data;

import com.teenthofabud.core.common.data.document.TOABBaseDocument;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString(onlyExplicitlyIncluded = true)
@Document("category")
public class CategoryDocument extends TOABBaseDocument implements Comparable<CategoryDocument> {

    @Id
    private String id;
    @Indexed
    private String name;
    private String description;

    @Override
    public int compareTo(CategoryDocument o) {
        return this.getId().compareTo(o.getId());
    }
}

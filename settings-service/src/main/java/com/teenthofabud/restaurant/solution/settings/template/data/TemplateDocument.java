package com.teenthofabud.restaurant.solution.settings.template.data;

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
@Document("template")
public class TemplateDocument extends TOABBaseDocument implements Comparable<TemplateDocument> {

    @Id
    @Indexed
    private String id;
    private String name;
    private String description;
    private String templateTypeId;
    private String content;

    @Override
    public int compareTo(TemplateDocument o) {
        return this.getId().compareTo(o.getId());
    }
}

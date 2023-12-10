package com.teenthofabud.restaurant.solution.settings.internationalization.data;

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
@Document("language")
public class LanguageDocument extends TOABBaseDocument implements Comparable<LanguageDocument> {

    @Id
    @Indexed
    private String id;
    private String code;
    private String name;
    private String idiom;
    private Boolean canDelete;


    @Override
    public int compareTo(LanguageDocument o) {
        return this.getId().compareTo(o.getId());
    }
}

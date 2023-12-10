package com.teenthofabud.restaurant.solution.settings.internationalization.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class LanguageForm {
    @ToString.Include
    private String name;
    @ToString.Include
    private String idiom;
    @ToString.Include
    private String code;
    @ToString.Include
    private boolean deletable;
}

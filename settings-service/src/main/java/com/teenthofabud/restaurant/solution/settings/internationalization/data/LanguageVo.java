package com.teenthofabud.restaurant.solution.settings.internationalization.data;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.teenthofabud.core.common.data.vo.TOABBaseVo;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
@ToString(onlyExplicitlyIncluded = true)
public class LanguageVo extends TOABBaseVo implements Comparable<LanguageVo> {

    @EqualsAndHashCode.Include
    @ToString.Include
    private String id;
    @ToString.Include
    private String code;
    @ToString.Include
    private String name;
    @ToString.Include
    private String idiom;
    @ToString.Include
    private Boolean canDelete;

    @Override
    public int compareTo(LanguageVo o) {
        //TODO -- Need to confirm as in the doc it is mentioned that we need to sort the object based on the FirstName and LastName
        return this.getName().compareTo(o.getName());
    }
}

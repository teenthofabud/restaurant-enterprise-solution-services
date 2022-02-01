package com.teenthofabud.restaurant.solution.session.association.converter;

import com.teenthofabud.core.common.constant.TOABCascadeLevel;
import com.teenthofabud.core.common.converter.TOABBaseDocument2VoConverter;
import com.teenthofabud.core.common.data.dto.TOABRequestContextHolder;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.restaurant.solution.customer.account.data.AccountVo;
import com.teenthofabud.restaurant.solution.session.association.data.AssociationDocument;
import com.teenthofabud.restaurant.solution.session.association.data.AssociationVo;
import com.teenthofabud.restaurant.solution.session.experience.data.ExperienceVo;
import com.teenthofabud.restaurant.solution.session.experience.service.ExperienceService;
import com.teenthofabud.restaurant.solution.session.integration.customer.account.proxy.AccountServiceClient;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.data.TableVo;
import com.teenthofabud.restaurant.solution.session.integration.establishmentarea.table.proxy.TableServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.convert.converter.Converter;
import org.springframework.scheduling.concurrent.CustomizableThreadFactory;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.*;

@Component
@Slf4j
public class AssociationDocument2VoConverter extends TOABBaseDocument2VoConverter<AssociationDocument, AssociationVo> implements Converter<AssociationDocument, AssociationVo> {

    private List<String> fieldsToEscape;
    private TableServiceClient tableServiceClient;
    private AccountServiceClient accountServiceClient;
    private ExperienceService experienceService;


    @Value("#{'${res.session.association.fields-to-escape}'.split(',')}")
    public void setFieldsToEscape(List<String> fieldsToEscape) {
        this.fieldsToEscape = fieldsToEscape;
    }

    @Autowired
    public void setTableServiceClient(TableServiceClient tableServiceClient) {
        this.tableServiceClient = tableServiceClient;
    }

    @Autowired
    public void setAccountServiceClient(AccountServiceClient accountServiceClient) {
        this.accountServiceClient = accountServiceClient;
    }

    @Autowired
    public void setExperienceService(ExperienceService experienceService) {
        this.experienceService = experienceService;
    }

    @Override
    public AssociationVo convert(AssociationDocument document) {
        AssociationVo vo = new AssociationVo();
        if(!fieldsToEscape.contains("id")) {
            vo.setId(document.getId().toString());
        }
        if(!fieldsToEscape.contains("experienceId")) {
            this.expandSecondLevelFields(document, vo, "experienceId");
        }
        if(!fieldsToEscape.contains("tableId")) {
            this.expandSecondLevelFields(document, vo, "tableId");
        }
        if(!fieldsToEscape.contains("accountId")) {
            this.expandSecondLevelFields(document, vo, "accountId");
        }
        /*if(!fieldsToEscape.contains("endedOn")) {
            vo.setEndedOn(document.getEndedOn());
        }*/
        super.expandAuditFields(document, vo);
        log.debug("Converted {} to {} ", document, vo);
        return vo;
    }

    private void expandSecondLevelFields(AssociationDocument document, AssociationVo vo, String fieldName) {
        TOABCascadeLevel cascadeLevel = TOABRequestContextHolder.getCascadeLevelContext();
        switch(cascadeLevel) {
            case TWO:
                if(!fieldsToEscape.contains("experienceId") && fieldName.compareTo("experienceId") == 0) {
                    Callable<ExperienceVo> recipeDocument2VoConversion = () -> {
                        ExperienceVo recipeVo = experienceService.retrieveDetailsById(document.getExperienceId(), Optional.of(TOABCascadeLevel.ZERO));
                        return recipeVo;
                    };
                    String tName = "experienceDocument2VoConversion";
                    ExecutorService executorService = Executors.newFixedThreadPool(1, new CustomizableThreadFactory("experienceDocument2VoConversion-"));
                    Future<ExperienceVo> experienceDocument2VoConversionResult = executorService.submit(recipeDocument2VoConversion);
                    try {
                        ExperienceVo experienceVo = experienceDocument2VoConversionResult.get();
                        vo.setExperience(experienceVo);
                        log.debug("Retrieved {} for experienceId: {}", experienceVo, document.getExperienceId());
                    } catch (InterruptedException | ExecutionException e) {
                        String msg = "Unable to perform " + tName;
                        log.error(msg, e);
                        throw new TOABSystemException(TOABErrorCode.SYSTEM_INTERNAL_ERROR, msg, new Object[] { tName + " failure: " + e.getMessage() });
                    }
                }
                if(!fieldsToEscape.contains("tableId") && fieldName.compareTo("tableId") == 0) {
                    TableVo table = tableServiceClient.getTableDetailsById(document.getTableId());
                    log.debug("Retrieved {} for table is: {}", table, document.getTableId());
                    vo.setTable(table);
                }
                if(!fieldsToEscape.contains("accountId") && fieldName.compareTo("accountId") == 0) {
                    AccountVo account = accountServiceClient.getAccountDetailsById(document.getAccountId());
                    log.debug("Retrieved {} for account is: {}", account, document.getTableId());
                    vo.setAccount(account);
                }
                break;
            default:
                vo.setExperienceId(document.getExperienceId());
                vo.setTableId(document.getTableId());
                vo.setAccountId(document.getAccountId());
                log.debug("only first level cascaded for association over experienceId, tableId and accountId");
                break;
        }
    }

}
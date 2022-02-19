package com.bcdm.foodtraceability.controller;


import com.bcdm.foodtraceability.configuration.BlogAction;
import com.bcdm.foodtraceability.entity.CompanyInfoCheck;
import com.bcdm.foodtraceability.entity.ReturnItem;
import com.bcdm.foodtraceability.service.CompanyInfoCheckService;
import org.springframework.web.bind.annotation.*;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.MODIFY_COMPANY_INFO_SUCCESS;

/**
 * <p>
 *  营业信息更改前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-02-14
 */
@RestController
@RequestMapping("/companyInfoCheck")
public class CompanyInfoCheckController {

    private final CompanyInfoCheckService companyInfoCheckService;

    public CompanyInfoCheckController(CompanyInfoCheckService companyInfoCheckService) {
        this.companyInfoCheckService = companyInfoCheckService;
    }

    /**
     * 企业营业执照更新Controller
     *
     * @param company 需要修改的公司信息
     * @return 修改成功的公司信息
     * @throws Exception 修改信息失败
     */
    @PostMapping("/modifyBusinessLicense")
    @CrossOrigin
    public ReturnItem<Boolean> modifyBusinessLicense(@RequestBody CompanyInfoCheck company) throws Exception {
        BlogAction.logger.info("用户:" + company.getUserId() + "-----更新" + company.getCompanyId() + "企业的营业执照信息");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(companyInfoCheckService.modifyBusinessLicense(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_COMPANY_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 企业经营许可证更新Controller
     *
     * @param company 需要修改的公司信息
     * @return 修改成功的公司信息
     * @throws Exception 修改信息失败
     */
    @PostMapping("/modifyHealthPermit")
    @CrossOrigin
    public ReturnItem<Boolean> modifyHealthPermit(@RequestBody CompanyInfoCheck company) throws Exception {
        BlogAction.logger.info("用户:" + company.getUserId() + "-----更新" + company.getCompanyId() + "企业的经营许可证信息");
        ReturnItem<Boolean> returnItem = new ReturnItem<>();
        returnItem.setT(companyInfoCheckService.modifyHealthPermit(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_COMPANY_INFO_SUCCESS);
        return returnItem;
    }

}


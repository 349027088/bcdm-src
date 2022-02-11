package com.bcdm.foodtraceability.controller;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.service.CompanyService;
import com.bcdm.foodtraceability.service.UserService;
import com.bcdm.foodtraceability.validatedgroup.CreateGroup;
import com.bcdm.foodtraceability.validatedgroup.GetInfoGroup;
import com.bcdm.foodtraceability.validatedgroup.ModifyGroup;
import com.bcdm.foodtraceability.validatedgroup.RegisterGroup;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import static com.bcdm.foodtraceability.common.HttpConstants.HTTP_RETURN_SUCCESS;
import static com.bcdm.foodtraceability.common.MessageConstants.*;

/**
 * <p>
 * 公司前端控制器
 * </p>
 *
 * @author 王
 * @since 2022-01-13
 */
@RestController
@RequestMapping("/company")
@Slf4j
public class CompanyController {

    private final UserService userService;

    private final CompanyService companyService;

    public CompanyController(UserService userService, CompanyService companyService) {
        this.userService = userService;
        this.companyService = companyService;
    }

    /**
     * 企业登录Controller
     *
     * @param company 新建企业的信息和新建企业的用户
     * @return 创建成功的企业信息
     * @throws Exception 创建失败
     */
    @PostMapping("/register")
    @CrossOrigin
    public ReturnItem<Company> register(@Validated({RegisterGroup.class})
                                        @RequestBody Company company) throws Exception {
        log.info("用户" + company.getUserId() + "-----创建新的企业");
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.register(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(CREATE_COMPANY_SUCCESS);
        return returnItem;
    }

    /**
     * 企业信息修改Controller
     *
     * @param company 需要修改的公司信息
     * @return 修改成功的公司信息
     * @throws Exception 修改信息失败
     */
    @PostMapping("/modify")
    @CrossOrigin
    public ReturnItem<Company> modify(@Validated({ModifyGroup.class})
                                      @RequestBody Company company) throws Exception {
        log.info("用户" + company.getUserId() + "-----修改" + company.getCompanyId() + "企业的信息");
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.modify(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_COMPANY_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 获得公司的用户所有信息
     *
     * @param selectInfo 获取用户信息的公司
     * @return 该公司的用户信息
     * @throws Exception 查询用户信息失败
     */
    @PostMapping("/getUserByCompany")
    @CrossOrigin
    public ReturnItem<IPage<UserModel>> getUserByCompany(@RequestBody String selectInfo) throws Exception {
        SelectPageEntity<UserModel> selectPageEntity = new SelectPageEntity<>(selectInfo);
        log.info("用户" + selectPageEntity.getUserId() + "-----获取" + selectPageEntity.getCompanyId() + "企业的员工信息");
        ReturnItem<IPage<UserModel>> returnItem = new ReturnItem<>();
        returnItem.setT(userService.getUserByCompany(selectPageEntity));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(COMPANY_USER_GET_SUCCESS);
        return returnItem;
    }

    /**
     * 获得对应的公司信息
     *
     * @param company 获取需要公司的信息
     * @return 找到的对应的公司信息
     * @throws Exception 查询用户信息失败
     */
    @PostMapping("/getCompanyInfo")
    @CrossOrigin
    public ReturnItem<Company> getCompanyInfo(@Validated({CreateGroup.class})
                                                             @RequestBody Company company) throws Exception {
        log.info("获取-----" + company.getCompanyId() + "企业的信息");
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.getCompanyInfo(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(GET_COMPANY_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 登录员工到企业
     *
     * @param company 登录的员工和企业信息
     * @return 被登录的企业信息
     * @throws Exception 登录员工到企业失败
     */
    @PostMapping("createUserToCompany")
    @CrossOrigin
    public ReturnItem<Company> createUserToCompany(@Validated({GetInfoGroup.class})
                                                   @RequestBody Company company) throws Exception {
        log.info("用户" + company.getUserId() + "-----申请加入" + company.getCompanyId() + "企业");
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.createUserToCompany(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(CREATE_USER_TO_COMPANY_SUCCESS);
        return returnItem;
    }

}


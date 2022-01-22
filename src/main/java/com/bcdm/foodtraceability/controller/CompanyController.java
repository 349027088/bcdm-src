package com.bcdm.foodtraceability.controller;

import com.alibaba.fastjson.JSONObject;
import com.bcdm.foodtraceability.entity.*;
import com.bcdm.foodtraceability.service.CompanyService;
import com.bcdm.foodtraceability.service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

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
     * @param jsonInfo 新建企业的信息和新建企业的用户
     * @return 创建成功的企业信息
     * @throws Exception 创建失败
     */
    @PostMapping("/register")
    @CrossOrigin
    public ReturnItem<Company> register(@RequestBody String jsonInfo) throws Exception {
        JSONObject jsonObject = JSONObject.parseObject(jsonInfo);
        User user = jsonObject.getObject("user", User.class);
        Company company = jsonObject.getObject("company", Company.class);
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.register(user, company));
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
    public ReturnItem<Company> modify(@RequestBody Company company) throws Exception {
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.modify(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(MODIFY_COMPANY_INFO_SUCCESS);
        return returnItem;
    }

    /**
     * 获得公司的用户所有信息
     *
     * @param company 获取用户信息的公司
     * @return 该公司的用户信息
     * @throws Exception 查询用户信息失败
     */
    @PostMapping("/getUserByCompany")
    @CrossOrigin
    public ReturnItem<List<UserModel>> getUserByCompany(@RequestBody Company company) throws Exception {
        ReturnItem<List<UserModel>> returnItem = new ReturnItem<>();
        returnItem.setT(userService.getUserByCompany(company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(COMPANY_USER_GET_SUCCESS);
        return returnItem;
    }

    /**
     * 登录员工到企业
     *
     * @param jsonInfo 登录的员工和企业信息
     * @return 被登录的企业信息
     * @throws Exception 登录员工到企业失败
     */
    @PostMapping("createUserToCompany")
    @CrossOrigin
    public ReturnItem<Company> createUserToCompany(@RequestBody String jsonInfo) throws Exception {
        JSONObject jsonObject = JSONObject.parseObject(jsonInfo);
        Company company = jsonObject.getObject("company", Company.class);
        Integer user_id = jsonObject.getInteger("userId");
        ReturnItem<Company> returnItem = new ReturnItem<>();
        returnItem.setT(companyService.createUserToCompany(user_id, company));
        returnItem.setHttpStatus(HTTP_RETURN_SUCCESS);
        returnItem.setHttpMessage(CREATE_USER_TO_COMPANY_SUCCESS);
        return returnItem;
    }

}


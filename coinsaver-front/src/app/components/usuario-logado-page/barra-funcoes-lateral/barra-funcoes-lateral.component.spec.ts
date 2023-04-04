import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BarraFuncoesLateralComponent } from './barra-funcoes-lateral.component';

describe('BarraFuncoesLateralComponent', () => {
  let component: BarraFuncoesLateralComponent;
  let fixture: ComponentFixture<BarraFuncoesLateralComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ BarraFuncoesLateralComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(BarraFuncoesLateralComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});

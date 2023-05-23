import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ModalTrocarSenhaComponent } from './modal-trocar-senha.component';

describe('ModalTrocarSenhaComponent', () => {
  let component: ModalTrocarSenhaComponent;
  let fixture: ComponentFixture<ModalTrocarSenhaComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ModalTrocarSenhaComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ModalTrocarSenhaComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
